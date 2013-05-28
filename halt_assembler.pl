:- module(halt_assembler, [execute/1]).
:- use_module(halt_machine).

:- op(700, xfy, :=).

%! <module> Assembler
%%
%% Given a sequence of machine instructions, perform them on the raw machine.

%% The table of operations
spec(and(A,B))          :- m(A) := m(A) \/ m(B).
spec(or(A,B))           :- m(A) := m(A) /\  m(B).
spec(exclusive_or(A,B)) :- m(A) := m(A) xor m(B).
spec(not(A))            :- m(A) := \ m(A).
spec(move(A,B))         :- m(A) := m(B).
spec(set(A,C))          :- m(A) := C.
spec(random(A))         :- m(A) := random.
spec(jump(X))           :- ip   := X.
spec(jump_if_zero(X,A)) :- (m(A) = 0) -> ip := X ; advance.
spec(halt)              :- halted := true.

%% Basic operations
evaluate(M, m(A),        V) :- halt_machine:get_register(M, A, V).
evaluate(M, A /\ B,      V) :- evaluate(M, A, RA), evaluate(M, B, RB),
                               V is RA /\ RB.
evaluate(M, A \/ B,      V) :- evaluate(M, A, RA), evaluate(M, B, RB),
                               V is RA \/ RB.
evaluate(M, A xor B,     V) :- evaluate(M, A, RA), evaluate(M, B, RB),
                               V is RA xor RB.
evaluate(M, \ A,         V) :- evaluate(M, A, RA), V is \ RA.
evaluate(M, A -> B ; C,  V) :- evaluate(M, A, RA),
                               RA -> evaluate(M, B, V) ; evaluate(M, C, V).
evaluate(M, m(A) := B,   V) :- evaluate(M, B, RB), set_register(M, A, RB, V).
evaluate(_, A,           A) :- number(A).
evaluate(_, random,      V) :- V is random(2).
evaluate(M, advance,     V) :- advance(M, V).
evaluate(M, A = B,       V) :- evaluate(M, A, RA), evaluate(M, B, RB),
                               RA = RB -> V = true ; V = false.

%% Special cases
evaluate(M, ip := X,        MA) :- set_instruction_pointer(M, X, MA).
evaluate(M, halted := true, MA) :- set_halted(M, MA).

%% Perform the specification
evaluate(M, X, V) :-
    clause(spec(X), Body), evaluate(M, Body, V).

%! execute(+Code)
%%
%%   Executes the sequence of assembler instructions, printing either
%%   'Program halts!' if the machine halts or runs out of
%%   instructions, otherwise printing 'Unable to determine if
%%   application halts' if we hit the 10,000th instruction without
%%   halting.
execute(Code) :- execute(Code, _).

%! execute(+Code, -FinalMachine)
%%
%%   Execute Code as a sequence of assembler instructions, unifying
%%   the final state of the machine with FinalMachine.
execute(Code, FinalMachine) :-
    halt_machine:initialize(Code, InitialMachine),
    execute_loop(InitialMachine, FinalMachine).

%% Executes a single instruction, then determine if we should loop or
%% not, looping if so.
execute_loop(Machine0, MachineN) :-
    execute_one(Machine0, Machine1),

    %% if we're halted, display the message; otherwise continue
    ((is_halted(Machine1) ; halt_machine:instruction_count(Machine1, 10000))
       -> do_halt(Machine1)
        ; execute_loop(Machine1, MachineN)).

%! execute_one(+InputMachine, -OutputMachine) is semidet.
%%
%%   Executes one instruction.
execute_one(Machine0, Machine1) :-
    %% fetch the next instruction
    halt_machine:next_instruction(Machine0, Inst),

    %% evaluate it
    evaluate(Machine0, Inst, Machine1).

%! do_halt(+Machine) is det.
%%
%%   Determine whether halt was artificial or natural and write to
%%   standard output accordingly.
do_halt(machine(_, _, _, _, true)) :-
    write('Program halts!'), nl.
do_halt(machine(Code, _, _, IP, _)) :-
    length(Code, CodeLength), CodeLength =< IP,
    write('Program halts!'), nl.
do_halt(machine(_, 10000, _, _, _)) :-
    write('Unable to determine if application halts.'), nl.
do_halt(_) :-
    write('Unable to determine if application halts (possible bug).'), nl.

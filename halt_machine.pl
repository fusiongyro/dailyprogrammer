:- module(halt_machine).

:- op(700, xfy, :=).

initialize(machine(0, Regs, 0, false)) :- length(Regs, 32), all_zeroes(Regs).

all_zeroes([0|Rest]) :- all_zeroes(Rest).
all_zeroes([]).

set_register(machine(InstCount0, Registers0, IP0, Halted),
	     Register,
	     Value,
	     machine(InstCount1, Registers1, IP1, Halted)) :-
    succ(InstCount0, InstCount1),
    succ(IP0, IP1),
    set_register(Register, Value, Registers0, Registers1).
set_register(0, Value, [_|Rest], [Value|Rest]).
set_register(N, Value, [X|Rest], [X|Remaining]) :-
    succ(N0, N),
    set_register(N0, Value, Rest, Remaining).

get_register(machine(_, Registers, _, _), Register, Value) :-
    nth0(Register, Registers, Value).

set_instruction_pointer(machine(IC, Instructions, _, Halted),
			X,
			machine(IC, Instructions, X, Halted)).

set_halted(machine(IC0, Code, IP0, _),
	   machine(IC1, Code, IP1, true)) :-
    succ(IC0, IC1),
    succ(IP0, IP1).

%% The table of operations
spec(and(A,B))          :- m(A) := m(A) \/ m(B).
spec(or(A,B))           :- m(A) := m(A) /\  m(B).
spec(exclusive_or(A,B)) :- m(A) := m(A) xor m(B).
spec(not(A))            :- m(A) := \ m(A).
spec(move(A,B))         :- m(A) := m(B).
spec(set(A,C))          :- m(A) := C.
spec(random(A))         :- m(A) := random.
spec(jump(X))           :- ip   := X.
spec(jump_if_zero(X,A)) :- m(A) -> ip := X.
spec(halt)              :- halted := true.

%% Basic operations
evaluate(M, m(A), V) :- get_register(M, A, V).
evaluate(M, A/\B, V) :- evaluate(M, A, RA), evaluate(M, B, RB), V is RA /\ RB.
evaluate(M, A\/B, V) :- evaluate(M, A, RA), evaluate(M, B, RB), V is RA \/ RB.
evaluate(M, A xor B, V) :- evaluate(M, A, RA), evaluate(M, B, RB), V is RA xor RB.
evaluate(M, \ A, V) :- evaluate(M, A, RA), V is \ RA.
evaluate(M, A -> B, V) :- evaluate(M, A, RA), RA -> evaluate(M, B, V).
evaluate(M, m(A) := B, V) :- evaluate(M, B, RB), set_register(M, A, RB, V).
evaluate(_, A, A) :- number(A).
evaluate(M, ip := X, MA) :- set_instruction_pointer(M, X, MA).
evaluate(M, halted := true, MA) :- set_halted(M, MA).
evaluate(_, random, V) :- V is random(2).

%% Perform the specification
evaluate(M, X, V) :-
    clause(spec(X), Body), evaluate(M, Body, V).

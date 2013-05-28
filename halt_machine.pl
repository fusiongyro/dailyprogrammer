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
    nth0(Registers, Register, Value).

set_instruction_pointer(machine(IC, Instructions, _, Halted),
			X,
			machine(IC, Instructions, X, Halted)).

set_halted(machine(IC0, Code, IP0, _),
	   machine(IC1, Code, IP1, true)) :-
    succ(IC0, IC1),
    succ(IP0, IP1).

spec(and(A,B))          :- m(A) := m(A) \/ m(B).
spec(or(A,B))           :- m(A) := m(A) /\  m(B).
spec(xor(A,B))          :- m(A) := m(A) xor m(B).
spec(not(A))            :- m(A) := \ m(A).
spec(move(A,B))         :- m(A) := m(B).
spec(set(A,C))          :- m(A) := C.
spec(random(A))         :- m(A) := random(2).
spec(jump(X))           :- ip   := X.
spec(jump_if_zero(X,A)) :- m(A) -> ip := X.
spec(halt)              :- halted := true.

execute(M, and(A,B), NM)           :- get_register(M, A, RA), get_register(M, B, RB), V is RA /\ RB, set_register(M, A, V, NM).
execute(M, or(A,B), NM)            :- get_register(M, A, RA), get_register(M, B, RB), V is RA \/ RB, set_register(M, A, V, NM).
execute(M, xor(A,B), NM)           :- get_register(M, A, RA), get_register(M, B, RB), V is RA xor RB, set_register(M, A, V, NM).
execute(M, not(A), NM)             :- get_register(M, A, RA), V is \ RA, set_register(M, A, V, NM).
execute(M, move(A,B), NM)          :- get_register(M, B, RB), set_register(M, A, RB, NM).
execute(M, set(A,C), NM)           :- set_register(M, A, C, NM).
execute(M, random(A), NM)          :- V is random(2), set_register(M, A, V, NM).
execute(M, jump(X), NM)            :- set_instruction_pointer(M, X, NM).
execute(M, jump_if_zero(X, A), NM) :- get_register(M, A, X),
				      X =:= 0 -> set_instruction_pointer(M, X, NM).
execute(M, halt, NM)               :- set_halted(M, NM).


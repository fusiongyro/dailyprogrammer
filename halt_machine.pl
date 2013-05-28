:- module(halt_machine, [initialize/2,
                         is_halted/1,
                         advance/2,
                         instruction_count/2,
                         set_register/4,
                         set_instruction_pointer/3,
                         set_halted/2]).

initialize(Code, machine(Code, 0, Regs, 0, false)) :- length(Regs, 32), all_zeroes(Regs).

advance(machine(Code, IC, Regs, IP0, Halted),
        machine(Code, IC, Regs, IP1, Halted)) :- succ(IP0, IP1).

all_zeroes([0|Rest]) :- all_zeroes(Rest).
all_zeroes([]).

set_register(machine(Code, InstCount0, Registers0, IP0, Halted),
	     Register,
	     Value,
	     machine(Code, InstCount1, Registers1, IP1, Halted)) :-
    succ(InstCount0, InstCount1),
    succ(IP0, IP1),
    set_register(Register, Value, Registers0, Registers1).
set_register(0, Value, [_|Rest], [Value|Rest]).
set_register(N, Value, [X|Rest], [X|Remaining]) :-
    succ(N0, N),
    set_register(N0, Value, Rest, Remaining).

get_register(machine(_, _, Registers, _, _), Register, Value) :-
    nth0(Register, Registers, Value).

set_instruction_pointer(machine(Code, IC0, Instructions, _, Halted),
			X,
			machine(Code, IC1, Instructions, X, Halted)) :-
    succ(IC0, IC1).

is_halted(machine(_, _, _, _, true)).

instruction_count(machine(_, IC, _, _, _), IC).

set_halted(machine(Code, IC0, Reg, IP0, _),
	       machine(Code, IC1, Reg, IP1, true)) :-
    succ(IC0, IC1),
    succ(IP0, IP1).

next_instruction(machine(Code, _, _, IP, _), Instruction) :-
    nth0(IP, Code, Instruction).

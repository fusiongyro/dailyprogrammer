:- module(halt_machine, [initialize/2,
                         is_halted/1,
                         advance/2,
                         instruction_count/2,
                         set_register/4,
                         set_instruction_pointer/3,
                         set_halted/2]).

%! <module> Halt machine
%%
%% Handles direct access to the machine itself.

%% The machine is a structure with the following shape:
%%
%%   machine(Code, InstructionCount, Registers, InstructionPointer, Halted)
%%
%%     - Code                 A list of instructions as defined by halt_parser
%%     - InstructionCount     An integer, the number of instructions executed
%%                            so far
%%     - Registers            A list containing 32 integer registers
%%     - InstructionPointer   An integer, the 0-based index of the next
%%                            instruction to run
%%     - Halted               Whether or not the machine is currently halted

%! initialize(+Code, -Machine) is det.
%%
%%   Unify Machine with a fresh virtual machine initialized with the
%%   supplied Code.
initialize(Code, machine(Code, 0, Regs, 0, false)) :-
    %% Initialize the registers
    length(Regs, 32), all_zeroes(Regs).

%! advance(+Machine, -Machine) is det.
%%
%%   Advance the instruction pointer
advance(machine(Code, IC, Regs, IP0, Halted),
        machine(Code, IC, Regs, IP1, Halted)) :- succ(IP0, IP1).

%! all_zeros(+List) is det.
%%
%%   Assert that all the elements of the list are zeroes.
all_zeroes([0|Rest]) :- all_zeroes(Rest).
all_zeroes([]).

%! set_register(+Machine, +Register, +Value, -Machine) is det.
%%
%%   Set the register numbered Register to contain Value in a new
%%   machine state. Also increments instruction count and instruction
%%   pointer.
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

%! get_register(+Machine, +Register, ?Value) is det.
%%
%%   Unify Value with the current value of the register numbered
%%   Register in the supplied Machine.
get_register(machine(_, _, Registers, _, _), Register, Value) :-
    nth0(Register, Registers, Value).

%! set_instruction_pointer(+Machine, +NewIP, -Machine) is det.
%%
%%   Set the instruction pointer in new copy of Machine to NewIP.
set_instruction_pointer(machine(Code, IC0, Instructions, _, Halted),
			X,
			machine(Code, IC1, Instructions, X, Halted)) :-
    succ(IC0, IC1).

%! is_halted(+Machine) is det.
%%
%%   True if Machine is in a halted state.
is_halted(machine(_, _, _, _, true)).

%! instruction_count(+Machine, ?IC) is det.
%%
%%   Unify the instruction counter of Machine with IC.
instruction_count(machine(_, IC, _, _, _), IC).

%! set_halted(+Machine, -Machine) is det.
%%
%%   Set the halted flag on the machine. Also increments the
%%   instruction counter and the instruction pointer.
set_halted(machine(Code, IC0, Reg, IP0, _),
	       machine(Code, IC1, Reg, IP1, true)) :-
    succ(IC0, IC1),
    succ(IP0, IP1).

%! next_instruction(+Machine, -Instruction) is det.
%%
%%   Unify the next instruction of Machine with Instruction.
next_instruction(machine(Code, _, _, IP, _), Instruction) :-
    nth0(IP, Code, Instruction).

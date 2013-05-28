:- module(halt_parser, [parse/2, parse_file/2]).
:- use_module(library(dcg/basics)).

%! <module> Parse the assembler code file
%%
%% Our intermediate representation is encoded directly in the
%% instruction patterns below.

int(X) --> integer(X).

instruction(and(A,B))           --> "AND", whites, int(A), whites, int(B).
instruction(or(A,B))            --> "OR",  whites, int(A), whites, int(B).
instruction(exclusive_or(A,B))  --> "XOR", whites, int(A), whites, int(B).
instruction(not(A))             --> "NOT", whites, int(A).
instruction(move(A,B))          --> "MOV", whites, int(A), whites, int(B).
instruction(set(A,C))           --> "SET", whites, int(A), whites, int(C).
instruction(random(A))          --> "RANDOM", whites, int(A).
instruction(jump(X))            --> "JMP", whites, int(X).
instruction(jump_if_zero(X, A)) --> "JZ", whites, int(X), whites, int(A).
instruction(halt)               --> "HALT".

instructions([Inst|Instructions]) -->
    instruction(Inst),
    blanks,
    !,
    instructions(Instructions).
instructions([]) --> [].

program(Program) --> integer(_), blanks, instructions(Program).

%! parse(+Input, -Program) is semidet.
%%
%%   Parse string Input into a sequence of code statements.
parse(Input, Program) :- phrase(program(Program), Input).

%! parse_file(+Filename, -Program) is semidet.
%%
%%   Parse the file specified by Filename into a sequence of code
%%   statements.
parse_file(Filename, Program) :-
    pio:phrase_from_file(halt_parser:program(Program), Filename).

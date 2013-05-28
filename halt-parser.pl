:- module(halt_parser, [parse/2, parse_file/2]).
:- use_module(library(dcg/basics)).

instruction(and(A,B))           --> "AND", whites, integer(A), whites, integer(B).
instruction(or(A,B))            --> "OR",  whites, integer(A), whites, integer(B).
instruction(exclusive_or(A,B))  --> "XOR", whites, integer(A), whites, integer(B).
instruction(not(A))             --> "NOT", whites, integer(A).
instruction(move(A,B))          --> "MOV", whites, integer(A), whites, integer(B).
instruction(set(A,C))           --> "SET", whites, integer(A), whites, integer(C).
instruction(random(A))          --> "RANDOM", whites, integer(A).
instruction(jump(X))            --> "JMP", whites, integer(X).
instruction(jump_if_zero(X, A)) --> "JZ", whites, integer(X), whites, integer(A).
instruction(halt)               --> "HALT".

instructions([Inst|Instructions]) -->
    instruction(Inst),
    blanks,
    !,
    instructions(Instructions).
instructions([]) --> [].

program(Program) --> integer(_), blanks, instructions(Program).

parse(Input, Program) :- phrase(program(Program), Input).
parse_file(Filename, Program) :-
    pio:phrase_from_file(halt_parser:program(Program), Filename).

:- module(halt, [main/0]).
:- use_module(library(optparse)).
:- use_module([halt_parser, halt_assembler]).

main :-
    opt_arguments([], _, [Arg]),
    process_file(Arg).

process_file(File) :-
    halt_parser:parse_file(File, Code),
    halt_assembler:execute(Code).


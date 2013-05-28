:- module(halt, [main/0]).
:- use_module(library(optparse)).
:- use_module([halt_parser, halt_assembler]).

%% Main entry point: run a single argument.
main :-
    opt_arguments([], _, [Arg]),
    process_file(Arg).

%! process_file(+Filename) is semidet.
%%
%%    Attempt to execute the machine using the code in the specified
%%    file.
process_file(File) :-
    halt_parser:parse_file(File, Code),
    halt_assembler:execute(Code).


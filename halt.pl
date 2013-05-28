:- module(halt, [main/0]).
:- use_module(library(optparse)).

main :-
    opt_arguments([], _, [Arg]),
    process_file(Arg).

process_file(Arg) :-
    write('Reading '), write(Arg), nl.

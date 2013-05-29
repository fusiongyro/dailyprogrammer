## [Halt! It's simulation time!](http://www.reddit.com/r/dailyprogrammer/comments/1euacb/052213_challenge_125_intermediate_halt_its/)
   
A brief tour of the simulation. The main function looks like this:

    :::prolog
    halt_parser:parse_file(File, Code),
    halt_assembler:execute(Code).

The `halt_parser` is a straightforward DCG. The assembler is a little
more interesting. The spec has been translated directly into code like
this:

    :::prolog
    spec(move(A,B)) :- m(A) := m(B).

The evaluator knows to look up specifications:

    :::prolog
    evaluate(M, X, V) :-
        clause(spec(X), Body), evaluate(M, Body, V).

It will then apply other rules to figure out how to evaluate the
specifications:

    :::prolog
    evaluate(M, m(A),        V) :- get_register(M, A, V).
    ...
    evaluate(M, m(A) := B,   V) :- evaluate(M, B, RB), set_register(M, A, RB, V).

These are all using machine primitives defined by `halt_machine`.

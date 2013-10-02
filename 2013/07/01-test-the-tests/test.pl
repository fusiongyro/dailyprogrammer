:- use_module(library(dcg/basics)).

%% Parsing
file(Tests) -->
    integer(N), blanks,
    { length(Tests, N) },
    tests(Tests).

tests([]) --> [].
tests([Test|Tests]) --> test(Test), blanks, tests(Tests).

test(reverse(X, Y)) -->
    "0 ",
    string_without(" ", X), white,
    string_without(" \n", Y).
test(upcase(X, Y)) -->
    "1 ",
    string_without(" ", X), white,
    string_without(" \n", Y).


%% Testing
upcase(X, Y) :- maplist(to_upper, X, Y).

%% Perform a test
test(Test) :-
    call(Test) -> (write('Good test data'), nl)
                ; (write('Mismatch! Bad test data'), nl).

%% Main
main(Filename) :-
    pio:phrase_from_file(file(Tests), Filename),
    maplist(test, Tests),
    !.

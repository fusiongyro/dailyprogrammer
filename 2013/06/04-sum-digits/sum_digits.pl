:- module(sum_digits, [sum/2, digits/2, sum_digits/2, main/0]).

sum(L, Sum) :- sum(0, L, Sum).
sum(Acc, [X|Xs], Sum) :- Acc1 is Acc + X, sum(Acc1, Xs, Sum).
sum(Sum, [], Sum).

digits(N, Digits) :-
    atom_chars(N, NChars),
    maplist(atom_number, NChars, Digits).

sum_digits(N, Sum) :-
    digits(N, Digits),
    sum(Digits, Sum).

sum_the_digits_loop(N) :-
    sum_digits(N, Sum),
    write(Sum), nl,
    Sum > 9 -> sum_the_digits_loop(Sum)
             ; true.

main :-
    prompt(_, ''),
    read_line_to_codes(user_input, XCodes),
    atom_codes(X, XCodes),
    write(X), nl,
    sum_the_digits_loop(X).

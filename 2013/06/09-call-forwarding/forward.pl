:- module(forward, [document//1, rforward/4]).
:- use_module(library(dcg/basics)).

document(document(Rules, DayToTest)) -->
    number(_), blanks,
    rules(Rules), blanks,
    number(DayToTest), blanks.

rules([Rule|Rules]) --> rule(Rule), blanks, !, rules(Rules).
rules([]) --> [].

rule(rule(Source, Dest, StartDay, EndDay)) -->
    number(Source), whites,
    number(Dest), whites,
    number(StartDay), whites,
    number(EndDay), whites.

forward(Source, Dest, Day, Rules) :-
    member(rule(Source, Dest, Start, End), Rules),
    Day >= Start, Day =< End.

rforward(Source, [Dest], Day, Rules) :-
    forward(Source, Dest, Day, Rules).
rforward(Source, [Int|Rest], Day, Rules) :-
    forward(Source, Int, Day, Rules),
    rforward(Int, Rest, Day, Rules).

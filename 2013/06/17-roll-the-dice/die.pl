:- module(die, [parse/2, roll/2]).

:- use_module(library(dcg/basics)).

% Parse XdY
die(die(N,F))       --> integer(N), "d", integer(F).
% Parse XdY+N
die(die(N,F,Plus))  --> integer(N), "d", integer(F), "+", integer(Plus).
% Parse XdY-N
die(die(N,F,Minus)) --> integer(N), "d", integer(F), "-", integer(Plus),
                        { Minus is -Plus }.

%    parse(Dice:string, Result:die).
%
% parse Dice from a string into a Prolog intermediate representation.
parse(Dice, Parsed) :- phrase(die(Parsed), Dice).

%    roll(Die:die, Result:list) is det.
%
% Roll the dice specified by Die. The result is a list of
% randomly-generated numbers.
%
% First, two base cases: roll "0" dice
roll(die(0, _), [])    :- !.
roll(die(0, _, _), []) :- !.
% Roll a die, return the result.
roll(die(N, F), [R|Rs]) :-
    R is random(F) + 1,
    N0 is N-1, 
    roll(die(N0, F), Rs), !.
% Roll a die with modifier, return result
roll(die(N, F, Plus), [R|Rs]) :-
    R is random(F) + Plus + 1,
    N0 is N-1, 
    roll(die(N0, F, Plus), Rs), !.
% Parse the string, then roll the die it represents
roll(String, Results) :-
    is_list(String),
    parse(String, Dice), !,
    roll(Dice, Results).

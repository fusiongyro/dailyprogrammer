:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

:- op(600, xfx, @).

% Colomb's law: Force = (Particle 1's mass x Particle 2's mass) / Distance^2
force(particle(Point1, Mass1), particle(Point2, Mass2), Force) :-
    distance(Point1, Point2, Distance),
    Force is (Mass1 * Mass2) / Distance ^ 2.

% Distance is the root of the X and Y differences squared
distance(X1@Y1, X2@Y2, D) :-
    D is sqrt((X1-X2)^2 + (Y1-Y2)^2).

% The file contains just two particles
file(P1, P2) --> particle(P1), blanks, particle(P2), blanks.

% A particle is three numbers separated by whitespace
particle(particle(X@Y, M)) -->
    number(M), whites, number(X), whites, number(Y).

% This is essentially the main program
calculate(File, Result) :-
    phrase_from_file(file(P1, P2), File),
    force(P1, P2, Result).

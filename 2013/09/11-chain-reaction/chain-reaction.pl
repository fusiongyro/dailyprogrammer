:- use_module(library(dcg/basics)).

:- op(500, xfx, @).

% ------------------
% P A R S I N G
% ------------------

% Parsing the file
input(XMax@YMax, Particles) -->
    number(XMax), whites, number(YMax), blanks,
    particles(Particles).

particles([]) --> [].
particles([P|Ps]) --> particle(P), blanks, particles(Ps).

particle(particle(X@Y, Radius, PropagationDirections)) -->
    number(X), whites,
    number(Y), whites,
    number(Radius), whites,
    directions(PropagationDirections).

directions([]) --> [].
directions([D|Ds]) --> direction(D), directions(Ds).

direction(up) 	--> "u"	; 	"U".
direction(down) --> "d" 	; 	"D".
direction(left) 	--> "l" 	; 	"L".
direction(right)	--> "r" 	; 	"R".

% ------------------
% U T I L I T I E S
% ------------------

flip_length(Y, X) :- length(X, Y).

% ------------------
% G R A P H I C S
% ------------------

% Generate an empty grid
make_grid(X, Y, Grid) :-
    length(Grid, X),
    maplist(flip_length(Y), Grid).

% Lay out a grid of size X*Y with Particles.
grid(X, Y, Particles, Grid) :- make_grid(X, Y, Grid), grid(Particles, 0'A, Grid).

% Render particles onto the grid
grid([], _, _).
grid([particle(X@Y, _, _)|Rest], Label, Grid) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Label),
    NextLabel is Label + 1,
    grid(Rest, NextLabel, Grid).

% Display the grid
write_grid([]).
write_grid([Row|Rows]) :- write_row(Row), nl, write_grid(Rows), !.

% Display a row of the grid
write_row([]).
write_row([32|Row]) :- write(' '), write_row(Row).
write_row([X|Row]) :- format('~c', [X]), write_row(Row).

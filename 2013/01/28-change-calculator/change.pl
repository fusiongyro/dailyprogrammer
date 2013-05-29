:- module(change, [make/5, minimum_for/1]).
:- use_module(library(clpfd)).

/* From:
 * 
 *   http://www.reddit.com/r/dailyprogrammer/comments/17f3y2/012813_challenge_119_easy_change_calculator/
 */

/** make(-Quarters:integer, -Dimes:integer, -Nickels:integer,
 *       -Pennies:integer, +Sum:integer) is semidet.
 *  make(+Quarters:integer, +Dimes:integer, +Nickels:integer,
 *       +Pennies:integer, -Sum:integer) is semidet.
 *
 *   True if the quantity of coins adds up to the sum.
 */
make(Quarters, Dimes, Nickels, Pennies, Sum) :-
    % Constraint: we may have 0 to Total of quarters, dimes, nickels
    % and pennies.
    [Quarters, Dimes, Nickels, Pennies] ins 0..Sum,

    % Constraint: the sum is the total value of the change
    Sum #= 25 * Quarters + 10 * Dimes + 5 * Nickels + Pennies,

    % Constraint: the total number of coins is the coin count
    sum([Quarters, Dimes, Nickels, Pennies], #=, Count),

    % Label minimizing the count
    once(labeling([min(Count)], [Quarters, Dimes, Nickels, Pennies, Count])).

/** minimum_for(+Amount) is det.
 *
 *   Display the minimum change that can be created for this amount of
 *   money.
 */
minimum_for(Amount) :-
    % convert it to an integer (cents)
    Amount1 is floor(Amount*100),

    % make change
    make(Q,D,N,P, Amount1),
    
    % display results
    write('Quarters: '), write(Q), nl,
    write('Dimes: '),    write(D), nl,
    write('Nickels: '),  write(N), nl,
    write('Pennies: '),  write(P), nl.

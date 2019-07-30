% Recall the min function:
min(X, Y, X) :- X =< Y. % clause 1
min(X, Y, Y) :- X > Y.  % clause 2
% Ex)
% ?- min(1, 2, M).
% M = 1 ;
% false.

% What we really want
% ?- min(1, 2, M).
% M = 1.

/*
  The problem is that it returns false after it succeeds with M = 1.

  The cause of this problem is that min(1, 2, M) satisfies 1, and then,
   if the user types ';', it keeps going and tries to satisfy clause 2,
   which fails and produces false.

  To fix this, we need some way to tell Prolog not to keep going after it
   succeeds in the first clause. The cut operator, '!', does exactly that.

  When Prolog encounters '!', it will not search for further ways to
   satisfy the query.
*/
mincut(X, Y, X) :- X =< Y, !. % clause 1
mincut(X, Y, Y) :- X > Y.     % clause 2

% Works as desires:
% ?- mincut(1, 2, M).
% M = 1.

% min_list has the same problem, even if we use mincut instead of min
min_list([X], X).             % base case
min_list([Head|Tail], Min) :- % recursive case
    min_list(Tail, Tmin),
    mincut(Head, Tmin, Min).
/* Example
?- min_list([4,3,5], M).
M = 3 ;
false.
*/

% This can be fixed by adding a cut to the recursive case
min_list_cut([X], X).             % base case
min_list_cut([Head|Tail], Min) :- % recursive case
    min_list_cut(Tail, Tmin),
    mincut(Head, Tmin, Min),
    !.
% Now:
% ?- min_list_cut([4,3,5], M).
% M = 3.

% negs(Nums, Negs) assigns to Negs all the numbers in Nums that are less
%  than 0.
% Note the placement of the cut:
%  The cut tells Prolog not to continue searching after clause 2 is
%   satisfied.
%  No cut is needed in the first clause because [] never matches
%   with a pattern like [H|T].
%  The third clause doesn't need a cut because there are no more clauses
%   after it.
negs([], []).             % clause 1
negs([N|Ns], [N|Rest]) :- % clause 2
    N < 0,
    negs(Ns, Rest),
    !. % cut added here
negs([_|Ns], Rest) :-
    negs(Ns, Rest).       % clause 3

% remove_all(X, Xs, Result) removes all occurrences of an element X from Xs.
remove_all(_, [], []).                         % base case
remove_all(X, [X|Rest], Result) :-             % recursive case
    remove_all(X, Rest, Result),
    !.
remove_all(X, [First|Rest], [First|Result]) :- % recursive case
    X \= First, % X doesn't unify with First
    remove_all(X, Rest, Result).

% Example
% ?- remove_all(5, [3,4,5,6,5], X).
% X = [3,4,6].

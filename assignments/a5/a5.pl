% 1. Implement makelist(N, X, Lst) that binds to Lst a new list
%    consisting of N copies of X. You can assume N is 0 or greater.
makelist(0, _, []).
makelist(N, X, [X|Xs]) :-
    N1 is N - 1,
    N1 >= 0,
    makelist(N1, X, Xs).

% 2. Implement second_min(Lst, M) that calculates the second smallest number
%    on a list. If the passed-in list has fewer than 2 elements it should
%    fail. You can assume Lst has no duplicates.
min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.

find_min([X], X).
find_min([H|T], Min) :-
    find_min(T, X),
    min(H, X, Min), !.

second_min([N1,N2|[]], M) :-
    max(N1, N2, M), !.

second_min(Lst, M) :-
    length(Lst, Len),
    Len >= 2,
    find_min(Lst, Min),
    delete(Lst, Min, Result),
    find_min(Result, M).

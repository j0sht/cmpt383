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
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.

second_min([N1,N2|[]], M) :-
    max(N1, N2, M), !.

second_min(Lst, M) :-
    min_list(Lst, Min),
    delete(Lst, Min, Result),
    min_list(Result, M).

% 3. Implement mynumlist(Lo, Hi, Result) that creates a list of numbers
%    from Lo to Hi. Don't use numlist.
mynumlist(Hi, Hi, [Hi]).
mynumlist(Lo, Hi, [Lo|Result]) :-
    Next is Lo + 1,
    Next =< Hi,
    mynumlist(Next, Hi, Result).

% 4. Implement the function all_diff(Lst) that succeeds (returns true)
%    just when Lst has no duplicate values. If Lst is empty, or only has
%    one element, then all_diff should succeed.
all_diff([]).
all_diff([_]).
all_diff([H|T]) :-
    \+ member(H, T),
    all_diff(T).

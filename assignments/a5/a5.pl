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

% 5. Implement negpos(L, Neg, NonNeg) that partitions a list L of numbers
%    into negatives and non-negatives. The order of the numbers in Neg
%    and NonNeg doesn't matter.
negpos([], [], []).
negpos([H|T], [H|As], B) :-
    H < 0,
    negpos(T, As, B).
negpos([H|T], A, [H|Bs]) :-
    H >= 0,
    negpos(T, A, Bs).

% 6. Implement magic(L9, Result) that takes a list L9 of 9 numbers as input,
%    and calculates a permutation of L9 that is magic.
magic(L9, Result) :-
    length(L9, Len),
    Len == 9,
    permutation(L9, Result),
    nth0(0, Result, A),
    nth0(1, Result, B),
    nth0(2, Result, C),
    nth0(3, Result, D),
    nth0(4, Result, E),
    nth0(5, Result, F),
    nth0(6, Result, G),
    nth0(7, Result, H),
    nth0(8, Result, I),
    sum_list([A,B,C], Row1),
    sum_list([D,E,F], Row2),
    sum_list([G,H,I], Row3),
    sum_list([A,D,G], Col1),
    sum_list([B,E,H], Col2),
    sum_list([C,F,I], Col3),
    Row1 == Row2,
    Row2 == Row3,
    Col1 == Col2,
    Col2 == Col3,
    Col3 == Row1.

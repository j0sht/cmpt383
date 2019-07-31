% 1. Implement makelist(N, X, Lst) that binds to Lst a new list
%    consisting of N copies of X.
makelist(1, X, [X]).
makelist(N, X, [X|Xs]) :-
    N1 is N - 1,
    N1 >= 1,
    makelist(N1, X, Xs).

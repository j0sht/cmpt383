% triples.pl

is_triple(A, B, C) :-
    D is C*C - A*A - B*B,
    D = 0.

solve_triple1(A, B, C) :-
    member(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    member(B, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    member(C, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    is_triple(A, B, C).

solve_triple2(A, B, C) :-
    member(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    member(B, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    A =< B,
    member(C, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    B =< C,
    is_triple(A, B, C).

solve_triple3(N, A, B, C) :-
    between(1, N, A),
    between(1, N, B),
    A =< B,
    between(1, N, C),
    B =< C,
    is_triple(A, B, C).

% triples.pl

% Three integers (a, b, c) form a Pythagorean triple if a^2 + b^2 = c^2
% is_triple tests for Pythagorean triples
is_triple(A, B, C) :-
    D is C*C - A*A - B*B,
    D = 0.

% Examples)
% ?- is_triple(3, 4, 5).
% true.
% ?- is_triple(4, 5, 7).
% false.

% solve_triple1 is used to generate solutions that form a Pythagorean triple
% This style of algorithm is sometimes referred to as "generate and test"
% We generate a candidate solution, and then test it to determine if its
% valid.
solve_triple1(A, B, C) :-
    member(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    member(B, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    member(C, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    is_triple(A, B, C).
% Example)
% ?- solve_triple1(A, B, C).
% A = 3,
% B = 4,
% C = 5 ;
% A = 4,
% B = 3,
% C = 5 ;
% A = 6,
% B = 8,
% C = 10 ;
% A = 8,
% B = 6,
% C = 10 ;
% false.

% In solve_triple1, four different Pythagorean triples are calculated.
% However, (3, 4, 5) and (4, 3, 5) are essentially the same (as are
%  (6, 8, 10), and (8, 6, 10))

% solve_triple2 requires that the triples be in order
solve_triple2(A, B, C) :-
    member(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    member(B, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    A =< B,
    member(C, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    B =< C,
    is_triple(A, B, C).
% Example)
% A = 3,
% B = 4,
% C = 5 ;
% A = 6,
% B = 8,
% C = 10 ;
% false.

% Limitation of solve_triple1 and 2 is that A, B, and C are limited
%  to numbers from 1 to 10.
% solve_triple3 generates triples from 1 to N
% Uses between(Lo, Hi, N) which generates integers from Lo to Hi (inclusive)
solve_triple3(N, A, B, C) :-
    between(1, N, A),
    between(1, N, B),
    A =< B,
    between(1, N, C),
    B =< C,
    is_triple(A, B, C).

bit(0).
bit(1).

nbits(1, [B]) :-
    bit(B).

nbits(N, [B|Bs]) :-
    N > 1,
    bit(B),
    N1 is N - 1,
    nbits(N1, Bs).

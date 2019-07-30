% A 4x4 Sudoku puzzle is played on a 4x4 grid and must satisfy the following:
%  - Each row and column must be a permutation of the numbers 1 to 4.
%  - The 4 2x2 sub-squares must be permutations of 1 to 4.
% solution finds a solution to the 4x4 Sudoku puzzle
solution(A, B, C, D,
	 E, F, G, H,
	 I, J, K, L,
	 M, N, O, P) :-
    % row constraints
    permutation([1,2,3,4], [A,B,C,D]),
    permutation([1,2,3,4], [E,F,G,H]),
    permutation([1,2,3,4], [I,J,K,L]),
    permutation([1,2,3,4], [M,N,O,P]),

    % column constraints
    permutation([1,2,3,4], [A,E,I,M]),
    permutation([1,2,3,4], [B,F,J,N]),
    permutation([1,2,3,4], [C,G,K,O]),
    permutation([1,2,3,4], [D,H,L,P]),

    % sub-square constraints
    permutation([1,2,3,4], [A,B,E,F]), % upper-left
    permutation([1,2,3,4], [C,D,G,H]), % upper-right
    permutation([1,2,3,4], [I,J,M,N]), % lower-left
    permutation([1,2,3,4], [K,L,O,P]). % lower-right

% The built-in function permutation generates permutations of the members
% of a list. Example)
% ?- permutation([1,2,3], Perm).
% Perm = [1, 2, 3] ;
% Perm = [1, 3, 2] ;
% Perm = [2, 1, 3] ;
% Perm = [2, 3, 1] ;
% Perm = [3, 1, 2] ;
% Perm = [3, 2, 1] ;
% false.

sudoku(A, B, C, D,
       E, F, G, H,
       I, J, K, L,
       M, N, O, P) :-
    solution(A, B, C, D,
	     E, F, G, H,
	     I, J, K, L,
	     M, N, O, P),
    nl,
    write('A solution to this puzzle is'),
    nl,
    printrow(A, B, C, D),
    printrow(E, F, G, H),
    printrow(I, J, K, L),
    printrow(M, N, O, P).

printrow(P, Q, R, S) :-
    write(' '), write(P), write(' '), write(Q),
    write(' '), write(R), write(' '), write(S), nl.

% Example)
/*
?- sudoku(
|    1, 4, _, _,
|    _, _, 4, _,
|    2, _, _, _,
|    _, _, _, _).

A solution to this puzzle is
 1 4 2 3
 3 2 4 1
 2 1 3 4
 4 3 1 2
true;

A solution to this puzzle is
 1 4 2 3
 3 2 4 1
 2 3 1 4
 4 1 3 2
true;

A solution to this puzzle is
 1 4 3 2
 3 2 4 1
 2 3 1 4
 4 1 2 3
true ;
false.
*/

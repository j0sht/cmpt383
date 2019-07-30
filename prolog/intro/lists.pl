/*
  Lists

  Lists begin with [ and end with ], values seperated by ','
  Ex) [5, 7, 3]

  Getting the first and rest of the list --> [Head|Rest] = [a,b,c,d]
  --> Head = a, Rest = [b,c,d]

  '|' is used frequently to process lists one element at a time

  You can also get more than one at time at the head of the list
  ?- [First, Second | Rest] = [a,b,c,d]
  First = a,
  Second = b,
  Rest = [c,d].
*/

/*
  Prolog rules are flexible enough to implement any function we like,
  so it is possible to use Prolog as a general-purpose programming language.

  Prolog has no loops, so we use recursion.
*/
% member(X, L)
% succeeds just when X is somewhere on the list L.

% The base case succeeds just when the item we are looking for is
%  the first element in the list.
% In the recursive case, we can assume the first element is not X because
%  it would have been caught by the first case, so if X is on the list,
%  it must be somewhere in Rest.
member(X, [X|_]).                       % base case
member(X, [_|Rest]) :- member(X, Rest). % recursive case

% Examples)
% ?- member(5, [6, 8, 15, 1]).
% false.
% ?- member(5, [6, 8, 5, 1]).
% true;  % user typed ';'
% false.
/*
  In the example above, prolog finds 5 near the end of the list, and the
  user types ';' so Prolog backtracks to its most recent decision point,
  and tries to find 5 somewhere later in the list. It can't, so it
  returns false.
*/
% Example) We can use member to iterate through the list
% ?- member(X, [6, 8, 1, 15]).
% X = 6 ;
% X = 8 ;
% X = 1 ;
% X = 15 ;
% false.

mylen([], 0).          % base case
mylen([_|Xs], Len) :-  % recursive case
    mylen(Xs, Rest_len),
    Len is 1 + Rest_len.
% Examples)
% ?- mylen([], N).
% N = 0.
% ?- mylen([3], N).
% N = 1.
% ?- mylen([3, 6, 8], N).
% N = 3.
% ?- mylen([3, X, 6, 8], N).
% N = 4.
% ?- mylen([3, 1, 4], 3).
% true.
% ?- mylen([3, 1, 4], 2).
% false.

sum([], 0).
sum([X|Xs], Total) :-
    sum(Xs, T),
    Total is X + T. % always use 'is' for arithmetic

mean(X, Avg) :-
    sum(X, Total),
    mylen(X, N),
    Avg is Total / N.

min(X, Y, X) :- X =< Y. % =< is less than or equal to
min(X, Y, Y) :- X > Y.

min_list([X], X).
min_list([Head|Tail], Min) :-
    min_list(Tail, Tmin),
    min(Head, Tmin, Min).

myappend([], Ys, Ys). % base case
myappend([X|Xs], Ys, [X|Zs]) :-
    myappend(Xs, Ys, Zs).

% Examples)
% ?- myappend([1,2], [3,4], [1,2,3,4]).
% true.
% ?- myappend([1,2], [3,4], [1,2,3,4,5]).
% false.
% ?- myappend([1,2], [3,4], X).
% X = [1,2,3,4].
% ?- myappend([1,2], X, [1,2,3,4]).
% X = [3,4].
% ?- myappend(X, [3,4], [1,2,3,4]).
% X = [1,2].
% ?- myappend(X, Y, [1,2,3,4]). % all pairs of lists that make [1,2,3,4]
% X = [],
% Y = [1,2,3,4] ;
% X = [1],
% Y = [2,3,4] ;
% X = [1,2],
% Y = [3,4] ;
% X = [1,2,3],
% Y = [4] ;
% X = [1,2,3,4],
% Y = [] ;
% false.

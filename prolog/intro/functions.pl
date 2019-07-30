% Prolog functions do not return values in the same way that functions
%  in most other programming languages do. You need to use extra parameters
%  to get thew results of a calculation.
to_celsius(F, C) :- C is (F - 32) * 5 / 9.

% The 'is' operator is used whenever we need the result of an arithmetic
%  calculation
% You call to_celsius like this:
% ?- to_celsius(90, C).
% C = 32.222222222222222.

% NOTE:
% Unfortunately, the first argument of to_celsius cannot be uninstantiated
% The right-hand side of the is statement is not allowed to have
%  uninstantiated variables

% Another function that calculates the area of a circle
circle_area(Radius, Area) :- Area is Radius * Radius * 3.14.

% Ex)
% ?- circle_area(3, Area).
% Area = 28.26.

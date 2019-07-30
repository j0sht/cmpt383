/*
  The ':-' in a Prolog rule means "if"
    - On the left of :- is the head of the rule
    - On the right is the body of the rule.
    - As with facts, a rule ends with a '.'
*/
male(bob).
male(doug).

female(val).
female(ada).

parents(doug, ada, bob).
parents(val, ada, bob).

% sister_of rule
% This can be read as:
%   "X is the sister of Y if X is female, and the mother and father
%    of X is the same as the mother and father of Y"
% Ex) ?- sister_of(val, doug). --> true
sister_of(X, Y) :-
    female(X),
    parents(X, Mother, Father),
    parents(Y, Mother, Father),
    X \= Y. % X \= Y succeeds only when X and Y don't unify w/ each other

/* Anonymous Variables

  - Prolog lets you use _ as a special anonymous variable
  - We use it when we must include a variable, but don't care about the
    value it is assigned

  - Ex) ?- sister_of(val, _). % is val the sister of someone?
     - Returns ttue.

  - Prolog tells us the question succeeded, but it does not corefer (share)
    with any other variable (even itself). So when sister_of(val, _), _
    will not share the same value as the X variable in the sister_of rule.
*/

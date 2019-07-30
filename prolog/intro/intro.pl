% intro.pl

% The following specifies a relationship between two objects:
%  The object 'john' and the object 'mary' are in the 'likes' relationship

/*
  NOTE:
    - The names of relationships and objects must start with lowercase
      letter.
    - In Prolog, uppercase letters are variables.
    - The 'likes' predicate is binary, i.e., it has an arity of 2. It has
      two arguments john, mary.
    - In general, the order of arguments, i.e. likes(john, mary). is not
      the same fact as likes(mary, john).
    - Object names, such as john and mary, are similar to symbols in Lisp:
        - They are not strings --> All we can do is test if two objects are
          the same or different (we can't access their individual characters)
    - The '.' character must always come at the end of a fact
*/
likes(john, mary). % a fact asserting that john likes mary

% More examples
fat(homer).                 % homer is fat
male(homer).                % homer is a male
father_of(homer, bart).     % homer is bart's father
kicked(itchy, scratchy).    % itchy kicked scratchy
stole(bart, donut, homer).  % bart stole the donut from homer
/*
  Lists of facts are often referred to as a database, or knowledge base (KB).
  Prolog is in some ways similar to a relational database system.

  Given a knowledge base, we can ask questions about it, i.e. query it.
  ex) ?- likes(john, mary). ==> true

    - Here likes(john, mary). is interpreted as a question
      "Does john like mary?"

  Prolog answers questions by searching the facts in its knowledge base
  to see if any match the question. If so, true is returned; if not,
  false is returned.
    - We say that a query succeeeds when it returns true, and fails when
      it returns false.
  
  Prolog uses an algorithm called unification to do its matching.
    - Two facts are said to unify if that have the same predicate name,
      the same number of arguments, and the same arguments in the same
      position.
    - For example) likes(john, mary). and likes(john, mary). unify, while
      likes(john, mary) and likes(mary, john) do not.
*/

% Variables
/*
  Variables in Prolog are called logic variables, and they are different than
  variables in other programming languages.
    - All prolog variables must start with an uppercase letter.
       - Ex) X, Who, John are all variables [x, who, john are not]
    - A Prolog variable is either instantiated or uninstantiated.
       - An instantiated variable has some value associated with it, while
         an uninstantiated variables has no value associated with it.
*/



indirect(A, B) :-
    subclassof(A,C),
    C \= A,
    subclassof(C,B), !.

indirect(A, B) :-
    subclassof(A,C),
    subclassof(C,D),
    D \= A,
    subclassof(D,B), !.

indirect(A, B) :-
    subclassof(A,C),
    subclassof(C,D),
    subclassof(D,E),
    E \= A,
    subclassof(E,B), !.

indirect(A, B) :-
    subclassof(A,C),
    subclassof(C,D),
    subclassof(D,E),
    subclassof(E,F),
    F \= A,
    subclassof(F,B), !.

indirect(A, B) :-
    subclassof(A,C),
    subclassof(C,D),
    subclassof(D,E),
    subclassof(E,F),
    subclassof(F,G),
    G \= A,
    subclassof(G,B), !.

indirect(A, B) :-
    subclassof(A,C),
    subclassof(C,D),
    subclassof(D,E),
    subclassof(E,F),
    subclassof(F,G),
    subclassof(G,H),
    H \= A,
    subclassof(H,B), !.

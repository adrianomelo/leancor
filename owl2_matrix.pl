create_matrix([], []).
create_matrix([Head|AxiomList], Ret) :-
    create_matrix(AxiomList, Matrix),
    to_clausule(Head, Clausule),
    append(Clausule, Matrix, Ret).

to_clausule(subClassOf(A, B), Matrix) :-
    to_clausule_left(A, Ad),
    to_clausule_right(B, Bd),
    append(Ad, Bd, M),
    nested(M, Matrix).

to_clausule(equivalentClasses(A, B), Matrix) :-
    to_clausule(subClassOf(A, B), Matrix1),
    to_clausule(subClassOf(B, A), Matrix2),
    append(Matrix1, Matrix2, Matrix).

to_clausule(inverseObjectProperties(A, B), [[NewA, -NewB], [-NewA, NewB]]) :-
    A=..[PropertyNameA,_,_],
    B=..[PropertyNameB,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameB, Y, X].

to_clausule(subObjectPropertyOf(A, B), [[NewA, -NewB], [-NewA, NewB]]) :-
    A=..[PropertyNameA,_,_],
    B=..[PropertyNameB,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameB, Y, X].

to_clausule_left(Exp, [M]) :-
    disjunction(Exp, A, B),
    to_clausule_left(A, Ad), to_clausule_left(B, Bd), append(Ad, Bd, M), !.

to_clausule_left(Exp, M) :-
    conjunction(Exp, A, B),
    to_clausule_left(A, Ad), to_clausule_left(B, Bd), append(Ad, Bd, M), !.

to_clausule_left(A, [A]) :-
    A=..[_, Arg1], atom_or_var(Arg1), !.

to_clausule_left(A, [A]) :- 
    A=..[_, Arg1, Arg2], atom_or_var(Arg1), atom_or_var(Arg2).

to_clausule_right(Exp, M) :-
    disjunction(Exp, A, B),
    to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.

to_clausule_right(Exp, [M]) :-
    conjunction(Exp, A, B),
    to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.

to_clausule_right(A, [-A]) :-
    A=..[_, Arg1], atom_or_var(Arg1), !.

to_clausule_right(A, [-A]) :-
    A=..[_, Arg1, Arg2], atom_or_var(Arg1), atom_or_var(Arg2).

atom_or_var(A) :- var(A).
atom_or_var(A) :- atom(A).

% to_clausule_right(union(A, B), M) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
% to_clausule_right(intersection(A, B), [M]) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
% to_clausule_right(objectAllValuesFrom(A, B), M) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
% to_clausule_right(objectSomeValuesFrom(A, B), [M]) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
% to_clausule_right(-A, [A]) :- !.
% to_clausule_right(A, [-A]).

nested_matrix([], []).
nested_matrix([Clausule|Clausules], Matrix) :-
    nested(Clausule, MatrixA),
    nested_matrix(Clausules, MatrixB),
    append(MatrixA, MatrixB, Matrix).

nested(Clausules, [Clausules]) :-
    get_nested(Clausules, Nested, _), Nested == [], !.
nested(Clausules, Matrix) :-
    get_nested(Clausules, Nested, NotNested),
    normalize(Nested, NotNested, Matrix1),
    nested_matrix(Matrix1, Matrix).
 
get_nested([], [], []).
get_nested([HeadIn|In], [HeadIn|NestedList], NotNested) :- 
    is_list(HeadIn),
    get_nested(In, NestedList, NotNested), !.
get_nested([HeadIn|In], NestedList, [HeadIn|NotNested]) :- 
    get_nested(In, NestedList, NotNested).

normalize([], Matrix, Matrix).
normalize([HeadNested|Nested], NotNested, Matrix) :-
    combine_clausules(HeadNested, NotNested, PartialMatrix),
    normalize(Nested, PartialMatrix, Matrix).

combine_clausules([], _, []).
combine_clausules([Literal|Clausules], NotNested, Matrix) :-
    append([Literal], NotNested, NewClausule),
    combine_clausules(Clausules, NotNested, PartialMatrix),
    append([NewClausule], PartialMatrix, Matrix).


conjunction(objectIntersectionOf(A, B), A, B).
conjunction(objectSomeValuesFrom(A, B), A, B).

disjunction(objectUnionOf(A, B), A, B).
disjunction(objectAllValuesFrom(A, B), A, B).


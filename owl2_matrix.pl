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

conjunction(objectIntersectionOf(A, B), A, B).
conjunction(objectSomeValuesFrom(A, B), A, B).

disjunction(objectUnionOf(A, B), A, B).
disjunction(objectAllValuesFrom(A, B), A, B).

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

atom_or_var(A) :- var(A), !.
atom_or_var(A) :- atom(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Axioms not related with Class Expressions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_clausule(inverseObjectProperties(A, B), [[NewA, -NewB], [-NewA, NewB]]) :-
    A=..[PropertyNameA,_,_],
    B=..[PropertyNameB,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameB, Y, X].

to_clausule(subObjectPropertyOf(A, B), [[NewA, -NewB]]) :-
    A=..[PropertyNameA,_,_],
    B=..[PropertyNameB,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameB, X, Y].

to_clausule(transitiveObjectProperty(A), [[NewA, NewB, -NewC]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameA, Y, Z],
    NewC=..[PropertyNameA, X, Z].

to_clausule(symmetricObjectProperty(A), [[NewA, -NewB]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameA, Y, X].

to_clausule(asymmetricObjectProperty(A), [[NewA, NewB]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameA, Y, X].

to_clausule(reflexiveObjectProperty(A), [[-NewA]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, X].

to_clausule(irreflexiveObjectProperty(A), [[NewA, eq(X, Y)]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y].

to_clausule(functionalObjectProperty(A), [[NewA, NewB, -eq(Y, Z)]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameA, X, Z].

% TODO: refactor the next 4 rules
to_clausule(objectPropertyDomain(Property, Class), [[NewProperty, -NewClass]]) :-
    Property=..[PropertyName,_,_],
    Class=..[ClassName,_],
    NewClass=..[ClassName,X],
    NewProperty=..[PropertyName,X,_].

to_clausule(dataPropertyDomain(Property, Class), [[NewProperty, -NewClass]]) :-
    Property=..[PropertyName,_,_],
    Class=..[ClassName,_],
    NewClass=..[ClassName,X],
    NewProperty=..[PropertyName,X,_].

to_clausule(objectPropertyRange(Property, Class), [[NewProperty, -NewClass]]) :-
    Property=..[PropertyName,_,_],
    Class=..[ClassName,_],
    NewClass=..[ClassName,X],
    NewProperty=..[PropertyName,_,X].

to_clausule(dataPropertyRange(Property, Class), [[NewProperty, -NewClass]]) :-
    Property=..[PropertyName,_,_],
    Class=..[ClassName,_],
    NewClass=..[ClassName,X],
    NewProperty=..[PropertyName,_,X].

to_clausule(classAssertion(A), [[-A]]).
to_clausule(objectPropertyAssertion(A), [[-A]]).
to_clausule(dataPropertyAssertion(A), [[-A]]).

%%%%%%%%%%%%%%%%%%
%% Helper Rules %%
%%%%%%%%%%%%%%%%%%

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

skolem_function(Function) :-
    skolemcounter(Counter),
    retractall(skolemcounter(_)),
    NewCounter is Counter+1,
    assert(skolemcounter(NewCounter)),
    atomic_list_concat([f, NewCounter], Function).

skolem_apply(Function, A, Fa) :-
    Fa=..[Function, A].

skolem_clear :-
    retractall(skolemcounter(_)),
    assert(skolemcounter(0)).

:- dynamic(skolemcounter/1).
:- assert(skolemcounter(0)).

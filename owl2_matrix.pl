:- [owl2_clausal].
:- [owl2_skolemization].

create_matrix([], []).
create_matrix([Head|AxiomList], Ret) :-
    create_matrix(AxiomList, Matrix),
    to_clausule(Head, Clausule),
    append(Clausule, Matrix, Ret).

conjunction(objectIntersectionOf(A, B), A, B).
conjunction(objectSomeValuesFrom(A, B), A, B).
conjunction(dataSomeValuesFrom(A, B), A, B).

disjunction(objectUnionOf(A, B), A, B).
disjunction(objectAllValuesFrom(A, B), A, B).
disjunction(dataAllValuesFrom(A, B), A, B).

to_clausule_left(Exp, [M]) :-
    disjunction(Exp, A, B),
    to_clausule_left(A, Ad),
    to_clausule_left(B, Bd),
    append([Ad], [Bd], M), !.

to_clausule_left(Exp, M) :-
    conjunction(Exp, A, B),
    to_clausule_left(A, Ad),
    to_clausule_left(B, Bd),
    append(Ad, Bd, M), !.

to_clausule_left(A, [A]) :-
    A=..[_, Arg1],
    atom_or_var(Arg1), !.

to_clausule_left(-A, [-A]) :-
    A=..[_, Arg1],
    atom_or_var(Arg1), !.

to_clausule_left(A, [A]) :- 
    A=..[_, Arg1, Arg2],
    atom_or_var(Arg1),
    atom_or_var(Arg2).

to_clausule_left(objectComplementOf(Exp), Matrix) :-
    to_clausule_left(-Exp, Matrix), !.

to_clausule_right(Exp, M, SkolemFunctions) :-
    disjunction(Exp, A, B),
    to_clausule_right(A, Ad, SkolemFunctions),
    to_clausule_right(B, Bd, SkolemFunctions),
    append(Ad, Bd, M), !.

to_clausule_right(objectSomeValuesFrom(A, B), [M], SkolemFunctions) :-
    to_clausule_apply_skolem(A, Ad, SkolemFunctions, NewSkolemFunctions),
    to_clausule_right(B, Bd, NewSkolemFunctions),
    append([[-Ad]], [Bd], M), !.

to_clausule_right(dataSomeValuesFrom(A, B), [M], SkolemFunctions) :-
    to_clausule_apply_skolem(A, Ad, SkolemFunctions, NewSkolemFunctions),
    to_clausule_right(B, Bd, NewSkolemFunctions),
    append([[-Ad]], [Bd], M), !.

to_clausule_right(Exp, [M], SkolemFunctions) :-
    conjunction(Exp, A, B),
    to_clausule_right(A, Ad, SkolemFunctions),
    to_clausule_right(B, Bd, SkolemFunctions),
    append([Ad], [Bd], M), !.

to_clausule_right(objectComplementOf(Exp), Matrix, SkolemFunctions) :-
    to_clausule_right(-Exp, Matrix, SkolemFunctions), !.

to_clausule_right(A, [-B], SkolemFunctions) :-
    A=..[Functor, Arg1],
    atom_or_var(Arg1),
    skolem_apply_list(SkolemFunctions, Arg1, Farg1),
    B=..[Functor, Farg1], !.

to_clausule_right(-A, [B], SkolemFunctions) :-
    A=..[Functor, Arg1],
    atom_or_var(Arg1),
    skolem_apply_list(SkolemFunctions, Arg1, Farg1),
    B=..[Functor, Farg1], !.

to_clausule_right(A, [-B], SkolemFunctions) :-
    A=..[Functor, Arg1, Arg2],
    atom_or_var(Arg1),
    atom_or_var(Arg2),
    skolem_apply_list(SkolemFunctions, Arg1, Farg1),
    skolem_apply_list(SkolemFunctions, Arg2, Farg2),
    B=..[Functor, Farg1, Farg2].

atom_or_var(A) :- var(A), !.
atom_or_var(A) :- atom(A).

to_clausule_apply_skolem(Property, NewProperty, SkolemFunctions, [Function|SkolemFunctions]) :-
    Property=..[Functor, A1, _],
    skolem_function(Function),
    skolem_apply(Function, A1, FA1),
    skolem_apply_list(SkolemFunctions, A1, B1),
    skolem_apply_list(SkolemFunctions, FA1, FB1),
    NewProperty=..[Functor, B1, FB1].

%%%%%%%%%%%%%%%%%
%% to_clausule %%
%%%%%%%%%%%%%%%%%

to_clausule(subClassOf(A, B), Matrix) :-
    to_clausule_left(A, Ad),
    to_clausule_right(B, Bd, []),
    append(Ad, Bd, M),
    nested(M, Matrix), !.

to_clausule(equivalentClasses(A, B), Matrix) :-
    to_clausule(subClassOf(A, B), Matrix1),
    to_clausule(subClassOf(B, A), Matrix2),
    append(Matrix1, Matrix2, Matrix), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Axioms not related with Class Expressions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_clausule(inverseObjectProperties(A, B), [[NewA, -NewB], [-NewA, NewB]]) :-
    A=..[PropertyNameA,_,_],
    B=..[PropertyNameB,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameB, Y, X], !.

to_clausule(subObjectPropertyOf(A, B), [[NewA, -NewB]]) :-
    A=..[PropertyNameA,_,_],
    B=..[PropertyNameB,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameB, X, Y], !.

to_clausule(transitiveObjectProperty(A), [[NewA, NewB, -NewC]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameA, Y, Z],
    NewC=..[PropertyNameA, X, Z], !.

to_clausule(symmetricObjectProperty(A), [[NewA, -NewB]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameA, Y, X], !.

to_clausule(asymmetricObjectProperty(A), [[NewA, NewB]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameA, Y, X], !.

to_clausule(reflexiveObjectProperty(A), [[-NewA]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, X], !.

to_clausule(irreflexiveObjectProperty(A), [[NewA, eq(X, Y)]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y], !.

to_clausule(functionalObjectProperty(A), [[NewA, NewB, -eq(Y, Z)]]) :-
    A=..[PropertyNameA,_,_],
    NewA=..[PropertyNameA, X, Y],
    NewB=..[PropertyNameA, X, Z], !.

to_clausule(objectPropertyDomain(Property, Class), Matrix) :- domain_range(Property, Class, Matrix), !.
to_clausule(dataPropertyDomain(Property, Class), Matrix) :- domain_range(Property, Class, Matrix), !.
to_clausule(objectPropertyRange(Property, Class), Matrix) :- domain_range(Property, Class, Matrix), !.
to_clausule(dataPropertyRange(Property, Class), Matrix) :- domain_range(Property, Class, Matrix), !.

to_clausule(classAssertion(A), [[-A]]) :- !.
to_clausule(objectPropertyAssertion(A), [[-A]]) :- !.
to_clausule(dataPropertyAssertion(A), [[-A]]) :- !.
to_clausule(class(_), []) :- !.
to_clausule(_, []).

domain_range(Property, Class, [[NewProperty, -NewClass]]) :-
    Property=..[PropertyName,_,_],
    Class=..[ClassName,_],
    NewClass=..[ClassName,X],
    NewProperty=..[PropertyName,X,_].

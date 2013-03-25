create_matrix([], []).
create_matrix([Head|AxiomList], Ret) :-
    to_clausule(Head, Clausule),
    append(Clausule, Matrix, Ret),
    create_matrix(AxiomList, Matrix).

to_clausule(subClassOf(A, B), Matrix) :-
    to_clausule_left(A, Ad),
    to_clausule_right(B, Bd),
    append(Ad, Bd, M),
    nested(M, Matrix).

% to_clausule(classAssertion(A), [[-A]]) :- !.
% to_clausule(propertyAssertion(A), [[-A]]) :- !.

% to_clausule(Item, []) :- \+Item=subClassOf(_,_), !.
% to_clausule(Item, []) :- \+Item=classAssertion(_), !.
% to_clausule(Item, []) :- \+Item=propertyAssertion(_), !.

to_clausule_left(objectUnionOf(A, B), [M]) :-
    to_clausule_left(A, Ad), to_clausule_left(B, Bd), append(Ad, Bd, M), !.

to_clausule_left(A, [A]) :- 
    functor(A, _, 1).

% to_clausule_left(intersection(A, B), M) :- to_clausule_left(A, Ad), to_clausule_left(B, Bd), append(Ad, Bd, M), !.
% to_clausule_left(objectSomeValuesFrom(A, B), M) :- to_clausule_left(A, Ad), to_clausule_left(B, Bd), append(Ad, Bd, M), !.
% to_clausule_left(A, [A]) :- !.

to_clausule_right(A, [-A]) :- functor(A, _, 1).
% to_clausule_right(union(A, B), M) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
% to_clausule_right(intersection(A, B), [M]) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
% to_clausule_right(objectAllValuesFrom(A, B), M) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
% to_clausule_right(objectSomeValuesFrom(A, B), [M]) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
% to_clausule_right(-A, [A]) :- !.
% to_clausule_right(A, [-A]).

nested(Clausules, [Clausules]) :-
    nested_clausules(Clausules, Nested, _), Nested == [], !.
nested(Clausules, Matrix) :-
    nested_clausules(Clausules, Nested, NotNested),
    normalize(Nested, NotNested, Matrix).

nested_clausules([], [], []).
nested_clausules([HeadIn|In], [HeadIn|NestedList], NotNested) :- 
    is_list(HeadIn),
    nested_clausules(In, NestedList, NotNested), !.
nested_clausules([HeadIn|In], NestedList, [HeadIn|NotNested]) :- 
    nested_clausules(In, NestedList, NotNested).

normalize([], Matrix, Matrix).
normalize([HeadNested|Nested], NotNested, Matrix) :-
    normalize2(HeadNested, NotNested, PartialMatrix),
    normalize(Nested, PartialMatrix, Matrix).

normalize2([], _, []).
normalize2([Literal|Clausules], NotNested, Matrix) :-
    append([Literal], NotNested, NewClausule),
    normalize2(Clausules, NotNested, PartialMatrix),
    append([NewClausule], PartialMatrix, Matrix).





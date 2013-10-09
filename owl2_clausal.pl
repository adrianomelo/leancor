
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

nested_matrix([], []).
nested_matrix([Clausule|Clausules], Matrix) :-
    nested(Clausule, MatrixA),
    nested_matrix(Clausules, MatrixB),
    append(MatrixA, MatrixB, Matrix).

combine_clausules([], _, []).
combine_clausules([Literal|Clausules], NotNested, Matrix) :-
    append([Literal], NotNested, NewClausule),
    combine_clausules(Clausules, NotNested, PartialMatrix),
    append([NewClausule], PartialMatrix, Matrix).

list_clausules(Input, Output) :- list_clausules(Input, [], Output).
list_clausules([], Clausules, Return) :-
    Return = Clausules, !.
list_clausules(Nested, Clausules, Return) :-
    Nested = [Head|List],
    Head = [Element|_],
    \+is_list(Element),
    append(Clausules, [Head], NewClausules),
    list_clausules(List, NewClausules, Return), !.
list_clausules(Nested, Clausules, Return) :-
    Nested = [Head|List],
    append(Head, List, NewNested),
    list_clausules(NewNested, Clausules, Return).


nested(Clausules, [Clausules]) :-
    get_nested(Clausules, Nested, _), Nested == [], !.
nested(Clausules, Matrix) :-
    get_nested(Clausules, NestedList, NotNested),
    normalize_nested_list(NestedList, [NotNested], Matrix).
 
get_nested([], [], []).
get_nested([HeadIn|In], [HeadIn|NestedList], NotNested) :- 
    is_list(HeadIn),
    get_nested(In, NestedList, NotNested), !.
get_nested([HeadIn|In], NestedList, [HeadIn|NotNested]) :- 
    get_nested(In, NestedList, NotNested).

normalize_nested_list([], Matrix, Matrix).
normalize_nested_list([HeadNestedList|NestedList], NotNested, PartialMatrix) :-
    normalize_nested_list(NestedList, NotNested, Matrix),
    list_clausules(HeadNestedList, HeadNestedListClean),
    normalize(HeadNestedListClean, Matrix, PartialMatrix).

normalize(_, [], []) :- !.
normalize(Nested, [HeadMatrix|Matrix], NewMatrix) :-
    combine_clausules(Nested, HeadMatrix, PartialMatrix),
    normalize(Nested, Matrix, MatrixA),
    append(PartialMatrix, MatrixA, NewMatrix).

combine_clausules([], _, []).
combine_clausules([Literal|Clausules], NotNested, Matrix) :-
    append(Literal, NotNested, NewClausule),
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

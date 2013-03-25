:- [owl2_matrix].
:- begin_tests(creatematrix).

test(subsumption1) :-
    Input  = [subClassOf(tasmaniandevil(X), marsupials(X))],
    Output = [[tasmaniandevil(X), -marsupials(X)]],
    create_matrix(Input, Output).

test(subsumption2) :-
    Input  = [subClassOf(tasmaniandevil(X), marsupials(X)), subClassOf(tasmaniandevil(X), marsupials(X))],
    Output = [[tasmaniandevil(X), -marsupials(X)], [tasmaniandevil(X), -marsupials(X)]],
    create_matrix(Input, Output).

test(lefthandsideunion) :-
    Input = [subClassOf(objectUnionOf(drancestor(Y), dr(Y)), drancestor(Y))],
    Output = [[drancestor(Y), -drancestor(Y)], [dr(Y), -drancestor(Y)]],
    create_matrix(Input, Output).

:- end_tests(creatematrix).
:- begin_tests(nested).

test(nested0) :-
    Input  = [tasmaniandevil(X), -marsupials(X)],
    Output = [[tasmaniandevil(X), -marsupials(X)]],
    nested(Input, Output).

test(nested1) :-
    Input  = [[drancestor(Y), dr(Y)], -drancestor(Y)],
    Output = [
        [drancestor(Y), -drancestor(Y)],
        [dr(Y), -drancestor(Y)]
    ],
    nested(Input, Output).

test(nestedclausules1) :-
    Clausules = [a,b,c,[d,e],f],
    nested_clausules(Clausules, Nested, NotNested),
    Nested = [[d, e]],
    NotNested = [a, b, c, f].

test(nestedclausules2) :-
    Clausules = [[drancestor(Y), dr(Y)], -drancestor(Y)],
    nested_clausules(Clausules, Nested, NotNested),
    Nested = [[drancestor(Y), dr(Y)]],
    NotNested = [-drancestor(Y)].

test(normalize21) :-
    HeadNested = [drancestor(Y), dr(Y)],
    NotNested = [-drancestor(Y)],
    normalize2(HeadNested, NotNested, Matrix).

:- end_tests(nested).
:- run_tests.
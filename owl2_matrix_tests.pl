
:- [owl2_matrix].
:- [owl2_parser].
:- begin_tests(to_clausule).

test(left_intersection1) :-
    Input  = subClassOf(objectIntersectionOf(a(X), b(X)), c(X)),
    to_clausule(Input, Output),
    Output == [[a(X), b(X), -c(X)]].

test(left_intersection2) :-
    Input  = subClassOf(objectIntersectionOf(a(X), objectIntersectionOf(b(X), c(X))), d(X)),
    to_clausule(Input, Output),
    Output == [[a(X), b(X), c(X), -d(X)]].

test(left_union1) :-
    Input  = subClassOf(objectUnionOf(a(X), b(X)), c(X)),
    to_clausule(Input, Output),
    Output == [[a(X), -c(X)], [b(X), -c(X)]].

test(left_union2) :-
    Input  = subClassOf(objectUnionOf(a(X), objectUnionOf(b(X), c(X))), d(X)),
    to_clausule(Input, Output),
    Output == [[a(X), -d(X)], [b(X), -d(X)], [c(X), -d(X)]].

test(left_union3) :-
    Input  = subClassOf(objectUnionOf(a(X), objectUnionOf(b(X), objectUnionOf(c(X), d(X)))), e(X)),
    to_clausule(Input, Output),
    Output == [[a(X), -e(X)], [b(X), -e(X)], [c(X), -e(X)], [d(X), -e(X)]].

test(left_intersection_union1) :-
    Input  = subClassOf(objectIntersectionOf(a(X), objectUnionOf(b(X), c(X))), d(X)),
    to_clausule(Input, Output),
    Output == [[b(X), a(X), -d(X)], [c(X), a(X), -d(X)]].

test(right_intersection1) :-
    Input  = subClassOf(a(X), objectIntersectionOf(b(X), c(X))),
    to_clausule(Input, Output),
    Output == [[-b(X), a(X)], [-c(X), a(X)]].

test(right_union1) :-
    Input  = subClassOf(a(X), objectUnionOf(b(X), c(X))),
    to_clausule(Input, Output),
    Output == [[a(X), -b(X), -c(X)]].

test(equivalent1) :-
    Input  = equivalentClasses(alsatianwine(X), objectIntersectionOf(abc(X), wine(X))),
    to_clausule(Input, Output),
    Output == [[-abc(X), alsatianwine(X)], [-wine(X), alsatianwine(X)], [abc(X), wine(X), -alsatianwine(X)]].

:- end_tests(to_clausule).
:- begin_tests(creatematrix).

test(subsumption1) :-
    Input  = [subClassOf(tasmaniandevil(X), marsupials(X))],
    create_matrix(Input, Output),
    Output == [[tasmaniandevil(X), -marsupials(X)]].

test(subsumption2) :-
    Input  = [subClassOf(tasmaniandevil(X), marsupials(X)), subClassOf(tasmaniandevil(X), marsupials(X))],
    create_matrix(Input, Output),
    Output == [[tasmaniandevil(X), -marsupials(X)], [tasmaniandevil(X), -marsupials(X)]].

test(lefthandsideunion1) :-
    Input = [subClassOf(objectUnionOf(drancestor(Y), dr(Y)), drancestor(Y))],
    create_matrix(Input, Output),
    Output == [[drancestor(Y), -drancestor(Y)], [dr(Y), -drancestor(Y)]].

test(lefthandsideunion2) :-
    Input = [subClassOf(objectUnionOf(drancestor(Y), objectUnionOf(dr(Y), person(Y))), drancestor(Y))],
    create_matrix(Input, Output),
    Output == [[drancestor(Y), -drancestor(Y)], [dr(Y), -drancestor(Y)], [person(Y), -drancestor(Y)]].

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

test(nested2) :-
    Input  = [[drancestor(Y), [dr(Y), person(Y)]], -drancestor(Y)],
    Output = [
        [drancestor(Y), -drancestor(Y)],
        [dr(Y), -drancestor(Y)],
        [person(Y), -drancestor(Y)]
    ],
    nested(Input, Output).

test(nested3) :-
    Input  = [[test1(Y), [test2(Y), test3(Y)]], -test4(Y)],
    Output = [
        [test1(Y), -test4(Y)],
        [test2(Y), -test4(Y)],
        [test3(Y), -test4(Y)]
    ],
    nested(Input, Output).

test(nestedclausules1) :-
    Clausules = [a,b,c,[d,e],f],
    get_nested(Clausules, Nested, NotNested),
    Nested == [[d, e]],
    NotNested == [a, b, c, f].

test(nestedclausules2) :-
    Clausules = [[drancestor(Y), dr(Y)], -drancestor(Y)],
    get_nested(Clausules, Nested, NotNested),
    Nested == [[drancestor(Y), dr(Y)]],
    NotNested == [-drancestor(Y)].

test(combine_clausules1) :-
    HeadNested = [drancestor(Y), dr(Y)],
    NotNested  = [-drancestor(Y)],
    combine_clausules(HeadNested, NotNested, Matrix),
    Matrix == [[drancestor(Y), -drancestor(Y)], [dr(Y), -drancestor(Y)]].

:- end_tests(nested).
:- begin_tests(complete).

test(axiom1) :-
    Input  = "EquivalentClasses(:AlsatianWine ObjectIntersectionOf(ObjectHasValue(:locatedIn :AlsaceRegion) :Wine))",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [
        [-locatedin(_, alsaceregion), alsatianwine(_)],
        [-wine(_), alsatianwine(_)],
        [locatedin(_, alsaceregion), wine(_), -alsatianwine(_)]
    ].

test(axiom2) :-
    Input  = "InverseObjectProperties(:producesWine :hasMaker)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [
        [produceswine(X, Y), -hasmaker(Y, X)],
        [-produceswine(X, Y), hasmaker(Y, X)]
    ].

test(axiom3) :-
    Input  = "SubObjectPropertyOf(:hasSugar :hasWineDescriptor)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[hassugar(X, Y), -haswinedescriptor(X, Y)]].

test(axiom4) :-
    Input  = "ClassAssertion(:Region :AlsaceRegion)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix == [[-region(alsaceregion)]].

test(axiom5) :-
    Input  = "ObjectPropertyDomain(:adjacentRegion :Region)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[adjacentregion(X, _), -region(X)]].

test(axiom6) :-
    Input  = "ObjectPropertyRange(:adjacentRegion :Region)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[adjacentregion(_, X), -region(X)]].

test(axiom7) :-
    Input  = "ObjectPropertyAssertion(:locatedIn :AlsaceRegion :FrenchRegion)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix == [[-locatedin(alsaceregion, frenchregion)]].

test(axiom8) :-
    Input  = "DataPropertyAssertion(:yearValue :Year1998 \"1998\"^^xsd:positiveInteger)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix == [[-yearvalue(year1998, '"1998"^^xsd:positiveInteger')]].

test(axiom9) :-
    Input  = "DataPropertyDomain(:yearValue :VintageYear)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[yearvalue(X, _), -vintageyear(X)]].

test(axiom10) :-
    Input  = "DataPropertyRange(:yearValue :positiveInteger)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[yearvalue(_, X), -positiveinteger(X)]].

test(axiom11) :-
    Input  = "SymmetricObjectProperty(:hasSugar)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[hassugar(X, Y), -hassugar(Y, X)]].

test(axiom12) :-
    Input  = "ReflexiveObjectProperty(:hasSugar)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[-hassugar(X, X)]].

test(axiom13) :-
    Input  = "TransitiveObjectProperty(:hasSugar)",
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[hassugar(X, Y), hassugar(Y,Z), -hassugar(X, Z)]].

test(axiom14) :-
    Input  = "AsymmetricObjectProperty(:hasSugar)", % aRb -> -bRa
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[hassugar(X, Y), hassugar(Y, X)]].

test(axiom15) :-
    Input  = "IrreflexiveObjectProperty(:hasSugar)", % aRb -> a!=b
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[hassugar(X, Y), eq(X, Y)]].

test(axiom16) :-
    Input  = "FunctionalObjectProperty(:hasSugar)", % aRb ^ aRc -> b=c
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[hassugar(X, Y), hassugar(X, Z), -eq(Y, Z)]].

test(axiom17) :-
    Input  = "SubClassOf(:Wine ObjectMaxCardinality(1 :madeFromGrape))", % aRA ^ aRB ->  A=B ..
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[wine(X), madefromgrape(X, A), madefromgrape(X, B), -eq(A,B)]].

test(axiom18) :-
    Input  = "SubClassOf(:Wine ObjectMaxCardinality(2 :madeFromGrape))", % aRA ^ aRB ^ aRC ->  A=B || B=C || A=C..
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [
        [wine(X), madefromgrape(X, A), madefromgrape(X, B),
         madefromgrape(X, C), -eq(A,B), -eq(A,C), -eq(B,C)]].

test(axiom19) :-
    Input  = "SubClassOf(:Wine ObjectMinCardinality(1 :madeFromGrape))", % aRA ^ aRB  ->  A!=B
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [[wine(X), madefromgrape(X, A), madefromgrape(X, B), -eq(A,B)]].

test(axiom20) :-
    Input  = "SubClassOf(:Wine ObjectMinCardinality(2 :madeFromGrape))", % aRA ^ aRB ^ aRC ->  A!=B && B!=C && A!=C..
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [
        [wine(X), madefromgrape(X, f1(_)),
            madefromgrape(X,f2(_)), madefromgrape(X,f3(_)), -eq(f1(_),f2(_))],
        [wine(X), madefromgrape(X, f1(_)),
            madefromgrape(X,f2(_)), madefromgrape(X,f3(_)), -eq(f2(_),f3(_))],
        [wine(X), madefromgrape(X, f1(_)),
            madefromgrape(X,f2(_)), madefromgrape(X,f3(_)), -eq(f1(_),f3(_))]
    ].

test(axiom21) :-
    Input  = "SubClassOf(:Wine ObjectExactCardinality(2 :madeFromGrape))", % aRA ^ aRB ^ aRC->  A!=B && (A=C || B=C)..
    axiom(Parsed, Input, []),
    create_matrix([Parsed], Matrix),
    Matrix = [
        [wine(X), madefromgrape(X, f1(_)),
            madefromgrape(X,f2(_)), madefromgrape(X,f3(_)), -eq(f1(_),f2(_))],
        [wine(X), madefromgrape(X, f1(_)),
            madefromgrape(X,f2(_)), madefromgrape(X,f3(_)), -eq(f2(_),f3(_))],
        [wine(X), madefromgrape(X, f1(_)),
            madefromgrape(X,f2(_)), madefromgrape(X,f3(_)), -eq(f1(_),f3(_))]
    ].

:- end_tests(complete).
:- run_tests.

% DONE
% EquivalentClasses, ObjectIntersectionOf, ObjectHasValue, 
% InverseObjectProperties, SubObjectPropertyOf, ObjectUnionOf,
% ObjectAllValuesFrom, ObjectSomeValuesFrom, SubClassOf
% ClassAssertion, ObjectPropertyDomain, ObjectPropertyRange
% ObjectPropertyAssertion, DataPropertyDomain, DataPropertyRange
% SymmetricObjectProperty, ReflexiveObjectProperty, TransitiveObjectProperty
% AsymmetricObjectProperty, IrreflexiveObjectProperty, FunctionalObjectProperty

% TO DO
% InverseFunctionalObjectProperty
% ObjectMaxCardinality, ObjectMinCardinality, ObjectExactCardinality
% ObjectOneOf, DisjointClasses, DifferentIndividuals

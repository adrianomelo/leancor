:- [leancop21_owl].

:- begin_tests(owlparser).

test(prefix) :-
    Input  = 'Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)',
    Output = prefix('rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>'),
    prefix(Output, [Input], []).

test(ontologyImport) :-
    Input  = 'Import(<http://www.co-ode.org/ontologies/pizza/pizza.owl>)',
    Output = import('<http://www.co-ode.org/ontologies/pizza/pizza.owl>'),
    import(Output, [Input], []).

test(classDeclaration) :-
    Input  = 'Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))',
    Output = animal(_),
    axiom(Output, [Input], []).
    
%test(individual) :-
%   Input  = 'ClassAssertion(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Degree> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#BA>)',
%   Output = degree(ba),
%   axiom(Output, [Input], []).

test(somevaluesfrom) :-
    Input  = 'ObjectSomeValuesFrom(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#hasHabitat> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#DryEucalyptForest>)',
    Output = objectSomeValuesFrom(hashabitat(X, Y), dryeucalyptforest(Y)),
    classExpression(Output, [Input], []).

test(allvaluesfrom) :-
    Input  = 'ObjectAllValuesFrom(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#hasHabitat> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#DryEucalyptForest>)',
    Output = objectAllValuesFrom(hashabitat(X, Y), dryeucalyptforest(Y)),
    classExpression(Output, [Input], []).

test(intersection1) :-
    Input  = 'ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB>)',
    Output = intersection(classa(_), classb(_)),
    classExpression(Output, [Input], []).

test(intersection2) :-
    Input  = 'ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassC>)',
    Output = intersection(classa(X), classb(X), classc(X)),
    classExpression(Output, [Input], []).

test(intersection3) :-
    Input  = 'ObjectIntersectionOf(ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>)',
    Output = intersection(objectSomeValuesFrom(haspart(_, Y), bone(Y)), animal(_)),
    classExpression(Output, [Input], []).
    
test(subClass1) :-
    Input = 'SubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)',
    Output = subClassOf(tasmaniandevil(X), marsupials(X)),
    axiom(Output, [Input], []).
    
test(subClass2) :-
    Input = 'SubClassOf(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Bird> ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal> ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Feather>)))',
    Output = subClassOf(bird(X), intersection(animal(X),
                                              objectSomeValuesFrom(haspart(X, Y), bone(Y)),
                                              objectSomeValuesFrom(haspart(X, Y), feather(Y)))),
    axiom(Output, [Input], []).

test(subClass3) :-
    Input  = 'SubClassOf(ObjectIntersectionOf(ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Vertebrate>)',
    Output = subClassOf(intersection(objectSomeValuesFrom(haspart(X, Y), bone(Y)), animal(_)), vertebrate(_)),
    axiom(Output, [Input], []).

test(disjoint) :-
    Input  = 'DisjointClasses(<http://www.cin.ufpe.br/~astm/owl/bird.owl#A> <http://www.cin.ufpe.br/~astm/owl/bird.owl#B>)',
    Output = disjoint(a(_), b(_)),
    axiom(Output, [Input], []).

test(disjoint2) :-
    Input  = 'DisjointClasses(<http://www.cin.ufpe.br/~astm/owl/bird.owl#A> <http://www.cin.ufpe.br/~astm/owl/bird.owl#B> <http://www.cin.ufpe.br/~astm/owl/bird.owl#C>)',
    Output = disjoint(a(_), disjoint(b(_), c(_))),
    axiom(Output, [Input], []).
    
:- end_tests(owlparser).
:- begin_tests(creatematrix).

test(class1) :-
    Input  = [class(classname(_))],
    Output = [],
    create_matrix(Input, Output).

test(subsumption1) :-
    Input  = [subClassOf(class(a(_)), class(b(_)))],
    Output = [[class(a(_)), -class(b(_))]],
    create_matrix(Input, Output).

test(subsumption2) :-
    Input  = [subClassOf(class(a(_)), class(b(_))), subClassOf(class(c(_)), class(d(_)))],
    Output = [[class(a(_)), -class(b(_))], [class(c(_)), -class(d(_))]],
    create_matrix(Input, Output).

test(disjunction1) :-
    Input  = [subClassOf(class(a(_)), -class(b(_)))],
    Output = [[class(a(_)), class(b(_))]],
    create_matrix(Input, Output).

test(disjunction2) :-
    Input  = [subClassOf(class(a(_)), -class(b(_))), subClassOf(class(c(_)), -class(d(_)))],
    Output = [[class(a(_)), class(b(_))], [class(c(_)), class(d(_))]],
    create_matrix(Input, Output).

test(lefthandsideunion1) :-
    Input  = [subClassOf(union(class(a(_)), class(b(_))), class(c(_)))],
    Output = [[class(b(_)), -class(c(_))], [class(a(_)), -class(c(_))]],
    create_matrix(Input, Output).

test(righthandsideunion1) :-
    Input  = [subClassOf(class(a(_)), union(class(b(_)), class(c(_))))],
    Output = [[class(a(_)), -class(b(_)), -class(c(_))]],
    create_matrix(Input, Output).

test(lefthandsideintersection1) :-
    Input  = [subClassOf(intersection(class(a(_)), class(b(_))), class(c(_)))],
    Output = [[class(a(_)), class(b(_)), -class(c(_))]],
    create_matrix(Input, Output).

test(righthandsideintersection1) :-
    Input  = [subClassOf(class(a(_)), intersection(class(b(_)), class(c(_))))],
    Output = [[class(a(_)), -class(c(_))], [class(a(_)), -class(b(_))]],
    create_matrix(Input, Output).

%test(lefthandsideexistencialrestriction1) :-
%   Input  = [subClassOf(objectSomeValuesFrom(objectProperty('p'), class('A')), class('B'))],
%   Output = [[class('A'), class('B'), -class('C')]],
%   create_matrix(Input, Output).

%test(righthandsideexistencialrestriction1) :-
%   Input  = [subClassOf(class('A'), objectSomeValuesFrom(objectProperty('p'), class('B')))],
%   Output = [[class('A'), class('B'), -class('C')]],
%   create_matrix(Input, Output).

%test(lefthandsideuniversalrestriction1) :-
%   Input  = [subClassOf(ObjectSomeValuesFrom(objectProperty('p'), class('A')), class('B'))],
%   Output = [[class('A'), class('B'), -class('C')]],
%   create_matrix(Input, Output).

%test(righthandsideuniversalrestriction1) :-
%   Input  = [subClassOf(class('A'), ObjectSomeValuesFrom(objectProperty('p'), class('B')))],
%   Output = [[class('A'), class('B'), -class('C')]],
%   create_matrix(Input, Output).

:- end_tests(creatematrix).
:- begin_tests(leancop).

:- [leancop_main].

test(owl1) :-
    parse_owl('testfiles/bird.owl', _, _, _, Axioms),
    create_matrix(Axioms, Matrix),
    prove(Matrix, Proof).

test(owl2) :-
    parse_owl('testfiles/cyclic.owl', _, _, _, Axioms),
    create_matrix(Axioms, Matrix),
    prove(Matrix, Proof).

:- end_tests(leancop).
:- run_tests.

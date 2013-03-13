:- [owl2_parser].

:- begin_tests(basic).

test(prefix) :-
    Input  = "Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)",
    Output = prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns'),
    prefix(Output, Input, []).

test(ontologyImport) :-
    Input  = "Import(<http://www.co-ode.org/ontologies/pizza/pizza.owl>)",
    Output = import('http://www.co-ode.org/ontologies/pizza/pizza.owl'),
    import(Output, Input, []).

test(classDeclaration1) :-
    Input  = "Declaration(NamedIndividual(<http://www.cin.ufpe.br/~astm/granparent.owl#pc>))",
    Output = namedIndividual(pc),
    declaration(Output, Input, []).

test(classDeclaration2) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))",
    Output = class(animal(_)),
    declaration(Output, Input, []).

test(individual) :-
    Input  = "ClassAssertion(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Degree> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#BA>)",
    Output = classAssertion(degree(ba)),
    classAssertion(Output, Input, []).

test(propertyassertion1) :-
   Input  = "ObjectPropertyAssertion(<http://www.cin.ufpe.br/~astm/cycles1.owl#hasSon> <http://www.cin.ufpe.br/~astm/cycles1.owl#Luiz> <http://www.cin.ufpe.br/~astm/cycles1.owl#Fred>)",
   Output = propertyAssertion(hasson(luiz,fred)),
   assertion(Output, Input, []).

test(somevaluesfrom1) :-
    Input  = "ObjectSomeValuesFrom(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#hasHabitat> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#DryEucalyptForest>)",
    Output = objectSomeValuesFrom(hashabitat(_,_), dryeucalyptforest(_)),
    classExpression(Output, Input, []).

test(somevaluesfrom2) :-
    Input  = "ObjectSomeValuesFrom(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#hasHabitat> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB>))",
    Output = objectSomeValuesFrom(hashabitat(_, Y), objectUnionOf(classa(Y), classb(Y))),
    classExpression(Output, Input, []).

test(allvaluesfrom) :-
    Input  = "ObjectAllValuesFrom(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#hasHabitat> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#DryEucalyptForest>)",
    Output = objectAllValuesFrom(hashabitat(_, Y), dryeucalyptforest(Y)),
    classExpression(Output, Input, []).

test(intersection1) :-
    Input  = "ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB>)",
    Output = objectIntersectionOf(classa(_), classb(_)),
    classExpression(Output, Input, []).

test(intersection2) :-
    Input  = "ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassC>)",
    Output = objectIntersectionOf(classa(_), objectIntersectionOf(classb(_), classc(_))),
    classExpression(Output, Input, []).

test(intersection3) :-
    Input  = "ObjectIntersectionOf(ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>)",
    Output = objectIntersectionOf(objectSomeValuesFrom(haspart(_, Y), bone(Y)), animal(_)),
    classExpression(Output, Input, []).

test(union1) :-
    Input  = "ObjectUnionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB>)",
    Output = objectUnionOf(classa(_), classb(_)),
    classExpression(Output, Input, []).

test(union2) :-
    Input  = "ObjectUnionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassC>)",
    Output = objectUnionOf(classa(_), objectUnionOf(classb(_), classc(_))),
    classExpression(Output, Input, []).

test(union3) :-
    Input  = "ObjectUnionOf(ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>)",
    Output = objectUnionOf(objectSomeValuesFrom(haspart(_, Y), bone(Y)), animal(_)),
    classExpression(Output, Input, []).
    
test(union4) :-
    Input  = "ObjectUnionOf(<http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor> <http://www.cin.ufpe.br/~astm/cycles1.owl#Dr>)",
    Output = objectUnionOf(drancestor(_), dr(_)),
    classExpression(Output, Input, []).

test(subClass1) :-
    Input = "SubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)",
    Output = subClassOf(tasmaniandevil(X), marsupials(X)),
    axiom(Output, Input, []).

test(subClass2) :-
    Input = "SubClassOf(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Bird> ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal> ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Feather>)))",
    Output = subClassOf(bird(X),objectIntersectionOf(animal(X),objectIntersectionOf(objectSomeValuesFrom(haspart(X,Y), bone(Y)),objectSomeValuesFrom(haspart(X,Y), feather(Y))))),
    axiom(Output, Input, []).

test(subClass3) :-
    Input  = "SubClassOf(ObjectIntersectionOf(ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Vertebrate>)",
    Output = subClassOf(objectIntersectionOf(objectSomeValuesFrom(haspart(_, Y), bone(Y)), animal(_)), vertebrate(_)),
    axiom(Output, Input, []).

test(gci1) :-
    Input = "SubClassOf(ObjectSomeValuesFrom(<http://www.cin.ufpe.br/~astm/cycles1.owl#hasSon> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor> <http://www.cin.ufpe.br/~astm/cycles1.owl#Dr>)) <http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor>)",
    Output = subClassOf(objectSomeValuesFrom(hasson(X, Y), objectUnionOf(drancestor(Y), dr(Y))), drancestor(X)),
    axiom(Output, Input, []).

test(gci2) :-
    Input = "SubClassOf(ObjectUnionOf(<http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor> <http://www.cin.ufpe.br/~astm/cycles1.owl#Dr>) <http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor>)",
    Output = subClassOf(objectUnionOf(drancestor(Y), dr(Y)), drancestor(_)),
    axiom(Output, Input, []).

test(gci3) :-
    Input = "SubClassOf(ObjectSomeValuesFrom(<http://www.cin.ufpe.br/~astm/cycles1.owl#hasSon> <http://www.cin.ufpe.br/~astm/cycles1.owl#Dr>) <http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor>)",
    Output = subClassOf(objectSomeValuesFrom(hasson(X, Y),  dr(Y)), drancestor(X)),
    axiom(Output, Input, []).

test(disjoint) :-
    Input  = "DisjointClasses(<http://www.cin.ufpe.br/~astm/owl/bird.owl#A> <http://www.cin.ufpe.br/~astm/owl/bird.owl#B>)",
    Output = disjoint(a(_), b(_)),
    axiom(Output, Input, []).

test(disjoint2) :-
    Input  = "DisjointClasses(<http://www.cin.ufpe.br/~astm/owl/bird.owl#A> <http://www.cin.ufpe.br/~astm/owl/bird.owl#B> <http://www.cin.ufpe.br/~astm/owl/bird.owl#C>)",
    Output = disjoint(a(_), disjoint(b(_), c(_))),
    axiom(Output, Input, []).

:- end_tests(basic).
:- begin_tests(combined).

test(prefixes1) :-
    Input = "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\nPrefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n",
    Prefixes = [prefix(owl, 'http://www.w3.org/2002/07/owl'), prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema')],
    prefixes(Prefixes, Input, _).

test(ontology1) :-
    Input = "Ontology(<http://www.cin.ufpe.br/~astm/or.owl>\n\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#A>))\n)",
    Imports = [],
    Axioms = [class(a(_))],
    ontology(Imports, Axioms, Input, []).

test(ontology2) :-
    Input = "Ontology(<http://cin.ufpe.br/~astm/owl/bird.owl>\nSubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)\n)",
    Imports = [],
    Axioms = [subClassOf(tasmaniandevil(_), marsupials(_))],
    ontology(Imports, Axioms, Input, []).

test(owl1) :-
    Input = "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\n\n\nOntology(<http://www.cin.ufpe.br/~astm/or.owl>\n\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#A>))\n)",
    Prefixes = [prefix(owl, 'http://www.w3.org/2002/07/owl')],
    Imports = [],
    Axioms = [class(a(_))],
    owl(Prefixes, Imports, Axioms, Input, []).

test(owl2) :-
    Input = "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\nPrefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)\nPrefix(xml:=<http://www.w3.org/XML/1998/namespace>)\nPrefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\nPrefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n\nOntology(<http://www.cin.ufpe.br/~astm/or.owl>\n\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#A>))\nSubClassOf(<http://www.cin.ufpe.br/~astm/or.owl#A> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/or.owl#C> <http://www.cin.ufpe.br/~astm/or.owl#B>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#B>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#C>))\n)",
    Prefixes = [prefix(owl, 'http://www.w3.org/2002/07/owl'), prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns'), prefix(xml, 'http://www.w3.org/XML/1998/namespace'), prefix(xsd, 'http://www.w3.org/2001/XMLSchema'), prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema')],
    Imports = [],
    Axioms = [
        class(a(_)),
        subClassOf(a(_), objectUnionOf(c(_), b(_))),
        class(b(_)),
        class(c(_))
    ],
    owl(Prefixes, Imports, Axioms, Input, _).

test(axioms1) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\n",
    Output = [class(animal(_))],
    axioms(Output, Input, []).

test(axioms2) :-
    Input  = "SubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)\nSubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)\n",
    Output = [subClassOf(tasmaniandevil(_), marsupials(_)), subClassOf(tasmaniandevil(_), marsupials(_))],
    axioms(Output, Input, []).

test(axioms3) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\n",
    Output = [class(animal(_)), class(animal(_)), class(animal(_))],
    axioms(Output, Input, []).

:- end_tests(combined).
:- run_tests.

%
%    Author:        Adriano S. T. de Melo
%    E-mail:        astm@cin.ufpe.br
%    WWW:           http://adrianomelo.com/
%    Copyright (C): 2013, Federal University of Pernambuco
%
%    This program is free software; you can redistribute it and/or
%    modify it under the terms of the GNU General Public License
%    as published by the Free Software Foundation; either version 2
%    of the License, or (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public
%    License along with this library; if not, write to the Free Software
%    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%
%    As a special exception, if you link this library with other files,
%    compiled with a Free Software compiler, to produce an executable, this
%    library does not by itself cause the resulting executable to be covered
%    by the GNU General Public License. This exception does not however
%    invalidate any other reasons why the executable file might be covered by
%    the GNU General Public License.
%

:- [owl2_parser].

:- begin_tests(basic).

test(prefix) :-
    Input  = "Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)",
    prefix(Output, Input, []),
    Output == prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns').

test(ontologyImport) :-
    Input  = "Import(<http://www.co-ode.org/ontologies/pizza/pizza.owl>)",
    import(Output, Input, []),
    Output == import('http://www.co-ode.org/ontologies/pizza/pizza.owl').

test(uri1) :-
    Input = "ObjectPropertyAssertion(:hasBody :VentanaCheninBlanc :Medium)",
    Output = objectPropertyAssertion(hasbody(ventanacheninblanc, medium)),
    axiom(Output, Input, []).

test(classDeclaration1) :-
    Input  = "Declaration(NamedIndividual(<http://www.cin.ufpe.br/~astm/granparent.owl#pc>))",
    Output = namedIndividual(pc),
    declaration(Output, Input, []).

test(classDeclaration2) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))",
    declaration(Output, Input, []),
    Output = class(animal(_)).

test(individual) :-
    Input  = "ClassAssertion(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Degree> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#BA>)",
    Output = classAssertion(degree(ba)),
    classAssertion(Output, Input, []).

test(propertyDomain) :-
    Input  = "ObjectPropertyDomain(<http://www.cin.ufpe.br/~astm/dataproperty#hasAbc> <http://www.cin.ufpe.br/~astm/dataproperty#C>)",
    Output = objectPropertyDomain(hasabc(_, _), c(_)),
    axiom(Output, Input, []).

test(propertyRange1) :-
    Input  = "ObjectPropertyRange(<http://www.cin.ufpe.br/~astm/dataproperty#hasAbc> <http://www.cin.ufpe.br/~astm/dataproperty#C>)",
    Output = objectPropertyRange(hasabc(_, _), c(_)),
    axiom(Output, Input, []).

test(propertyRange2) :-
    Input  = "ObjectPropertyRange(<http://www.cin.ufpe.br/~astm/dataproperty#hasAbc> ObjectSomeValuesFrom(<http://www.cin.ufpe.br/~astm/dataproperty#hasAbc> <http://www.cin.ufpe.br/~astm/dataproperty#C>))",
    Output = objectPropertyRange(hasabc(_, _), objectSomeValuesFrom(hasabc(_,_), c(_))),
    axiom(Output, Input, []).

test(inverseObjectProperty) :-
    Input  = "InverseObjectProperties(<http://www.cin.ufpe.br/~astm/dataproperty#hasCba> <http://www.cin.ufpe.br/~astm/dataproperty#hasAbc>)",
    Output = inverseObjectProperties(hascba(_, _), hasabc(_, _)),
    axiom(Output, Input, []).

test(symmetricObjectProperty) :-
    Input  = "SymmetricObjectProperty(<http://www.cin.ufpe.br/~astm/dataproperty#hasCba>)",
    Output = symmetricObjectProperty(hascba(_, _)),
    axiom(Output, Input, []).

test(propertyassertion1) :-
    Input  = "ObjectPropertyAssertion(<http://www.cin.ufpe.br/~astm/cycles1.owl#hasSon> <http://www.cin.ufpe.br/~astm/cycles1.owl#Luiz> <http://www.cin.ufpe.br/~astm/cycles1.owl#Fred>)",
    Output = objectPropertyAssertion(hasson(luiz,fred)),
    assertion(Output, Input, []).

test(propertyassertion2) :-
    Input  = "DataPropertyAssertion(<http://www.cin.ufpe.br/~astm/dataproperty#temNome> <http://www.cin.ufpe.br/~astm/dataproperty#c> \"nomeC\")",
    Output = dataPropertyAssertion(temnome(c,'\"nomeC\"')),
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

test(maxCardinality1) :-
    Input = "ObjectMaxCardinality(1 :madeFromGrape)",
    Output = objectMaxCardinality(1, madefromgrape),
    classExpression(Output, Input, []).

test(objectOneOf1) :-
    Input = "ObjectOneOf(:Peter :Lois :Stewie :Meg :Chris :Brian)",
    Output = objectOneOf(peter, objectOneOf(lois, objectOneOf(stewie, objectOneOf(meg, objectOneOf(chris, brian))))),
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

test(equivalent1) :-
    Input = "EquivalentClasses(<http://www.cin.ufpe.br/~astm/wine.owl#WineDescriptor> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/wine.owl#WineTaste> <http://www.cin.ufpe.br/~astm/wine.owl#WineColor>))",
    Output = equivalentClasses(winedescriptor(_), objectUnionOf(winetaste(_), winecolor(_))),
    axiom(Output, Input, []).

test(equivalent2) :-
    Input = "EquivalentClasses(:AlsatianWine ObjectIntersectionOf(ObjectHasValue(:locatedIn :AlsaceRegion) :Wine))",
    Output = equivalentClasses(alsatianwine(_), objectIntersectionOf(locatedin(_, alsaceregion), wine(_))),
    axiom(Output, Input, []).

test(equivalent3) :-
    Input  = "EquivalentClasses(:Meritage ObjectIntersectionOf(:Wine ObjectAllValuesFrom(:madeFromGrape ObjectOneOf(:PetiteVerdotGrape :MerlotGrape :MalbecGrape :CabernetSauvignonGrape :CabernetFrancGrape)) ObjectMinCardinality(2 :madeFromGrape)))",
    Output = equivalentClasses(meritage(_), objectIntersectionOf(wine(_), objectIntersectionOf(objectAllValuesFrom(madefromgrape(_, _), objectOneOf(petiteverdotgrape, objectOneOf(merlotgrape, objectOneOf(malbecgrape, objectOneOf(cabernetsauvignongrape, cabernetfrancgrape))))), objectMinCardinality(2, madefromgrape)))),
    axiom(Output, Input, []).

test(equivalent4) :-
    Input = "EquivalentClasses(:A ObjectIntersectionOf(:C :G ObjectUnionOf(:D :E :F) ObjectUnionOf(:I :H)))",
    Output = equivalentClasses(a(_), objectIntersectionOf(c(_), objectIntersectionOf(g(_), objectIntersectionOf(objectUnionOf(d(_), objectUnionOf(e(_), f(_))), objectUnionOf(i(_), h(_)))))),
    axiom(Output, Input, []).

test(disjoint) :-
    Input  = "DisjointClasses(<http://www.cin.ufpe.br/~astm/owl/bird.owl#A> <http://www.cin.ufpe.br/~astm/owl/bird.owl#B>)",
    Output = disjointClasses(a(_), b(_)),
    axiom(Output, Input, []).

test(disjoint2) :-
    Input  = "DisjointClasses(<http://www.cin.ufpe.br/~astm/owl/bird.owl#A> <http://www.cin.ufpe.br/~astm/owl/bird.owl#B> <http://www.cin.ufpe.br/~astm/owl/bird.owl#C>)",
    Output = disjointClasses(a(_), disjointClasses(b(_), c(_))),
    axiom(Output, Input, []).

test(subObjectPropertyOf1) :-
    Input  = "SubObjectPropertyOf(:hasBody :hasWineDescriptor)",
    Output = subObjectPropertyOf(hasbody(_,_), haswinedescriptor(_,_)),
    axiom(Output, Input, []).

test(differentIndividuals1) :- 
    Input  = "DifferentIndividuals(:Delicate :Moderate :Strong)",
    Output = differentIndividuals(delicate, differentIndividuals(moderate, strong)),
    axiom(Output, Input, []).

:- end_tests(basic).
:- begin_tests(combined).

test(prefixes1) :-
    Input = "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\nPrefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n",
    prefixes(Prefixes, Input, _),
    Prefixes = [prefix(owl, 'http://www.w3.org/2002/07/owl'), prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema')].

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
    owl(Prefixes, Imports, Axioms, Input, []),
    Prefixes == [prefix(owl, 'http://www.w3.org/2002/07/owl')],
    Imports == [],
    Axioms = [class(a(_))].

test(owl2) :-
    Input = "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\nPrefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)\nPrefix(xml:=<http://www.w3.org/XML/1998/namespace>)\nPrefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\nPrefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n\nOntology(<http://www.cin.ufpe.br/~astm/or.owl>\n\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#A>))\nSubClassOf(<http://www.cin.ufpe.br/~astm/or.owl#A> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/or.owl#C> <http://www.cin.ufpe.br/~astm/or.owl#B>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#B>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#C>))\n)",
    owl(Prefixes, Imports, Axioms, Input, _),
    Prefixes == [prefix(owl, 'http://www.w3.org/2002/07/owl'), prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns'), prefix(xml, 'http://www.w3.org/XML/1998/namespace'), prefix(xsd, 'http://www.w3.org/2001/XMLSchema'), prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema')],
    Imports == [],
    Axioms = [
        class(a(_)),
        subClassOf(a(_), objectUnionOf(c(_), b(_))),
        class(b(_)),
        class(c(_))
    ].

test(owl3) :-
    Input = "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\nPrefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)\nPrefix(xml:=<http://www.w3.org/XML/1998/namespace>)\nPrefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\nPrefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n\nOntology(<http://www.cin.ufpe.br/~astm/or.owl>\n\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#A>))\nSubClassOf(<http://www.cin.ufpe.br/~astm/or.owl#A> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/or.owl#C> <http://www.cin.ufpe.br/~astm/or.owl#B>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#B>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#C>))\n)\n",
    owl(Prefixes, Imports, Axioms, Input, []),
    Prefixes == [prefix(owl, 'http://www.w3.org/2002/07/owl'), prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns'), prefix(xml, 'http://www.w3.org/XML/1998/namespace'), prefix(xsd, 'http://www.w3.org/2001/XMLSchema'), prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema')],
    Imports == [],
    Axioms = [
        class(a(_)),
        subClassOf(a(_), objectUnionOf(c(_), b(_))),
        class(b(_)),
        class(c(_))
    ].

test(axioms1) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\n",
    Output = [class(animal(_))],
    axioms(Output, Input, []).

test(axioms2) :-
    Input  = "SubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)\nSubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)\n",
    axioms(Output, Input, []),
    Output = [subClassOf(tasmaniandevil(_), marsupials(_)), subClassOf(tasmaniandevil(_), marsupials(_))].

test(axioms3) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\n",
    axioms(Output, Input, []),
    Output = [class(animal(_)), class(animal(_)), class(animal(_))].

:- end_tests(combined).
:- run_tests.

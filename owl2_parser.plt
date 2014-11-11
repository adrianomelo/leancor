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
:- [owl2_operators].

% :- begin_tests(complex_tests).

% test(examples_from_specification) :-
%     Input = "SubClassOf(a:Man a:Person)
% EquivalentClasses(a:MongrelOwner a:Mongrel)
% DisjointClasses(a:Boy a:Girl)
% DisjointUnion(a:Child a:Boy a:Girl)
% SubObjectPropertyOf(a:hasDog a:hasPet)
% EquivalentObjectProperties(a:hasBrother a:hasMaleSibling)
% InverseObjectProperties(a:hasFather a:fatherOf)
% ObjectPropertyDomain(a:hasDog a:Person)
% ObjectPropertyRange(a:hasDog a:Dog)
% FunctionalObjectProperty(a:hasFather)
% InverseFunctionalObjectProperty(a:fatherOf)
% ReflexiveObjectProperty(a:knows)
% IrreflexiveObjectProperty(a:marriedTo)
% SymmetricObjectProperty(a:friend)
% AsymmetricObjectProperty(a:parentOf)
% TransitiveObjectProperty(a:ancestorOf)
% SubDataPropertyOf(a:hasLastName a:hasName)
% EquivalentDataProperties(a:hasName a:seLlama)
% DataPropertyDomain(a:hasName a:Person)
% DataPropertyRange(a:hasName xsd:string)
% FunctionalDataProperty(a:hasAge)
% ",
%     axioms(Output, Input, []),
%     Output = [
%         'Man' is_a 'Person',
%         'MongrelOwner' same_as 'Mongrel',
%         'Boy' disjoint_classes 'Girl',
%         'Child' disjoint_union ('Boy' disjoint_union 'Girl'),
%         hasDog subproperty hasPet,
%         equivalentObjectProperties(hasBrother(_G5352,_G5355),hasMaleSibling(_G5409,_G5412)),
%         hasFather inverse fatherOf,
%         hasDog domain 'Person',
%         hasDog range 'Dog',
%         functional hasFather,
%         inverseFunctionalObjectProperty(fatherOf(_G5638,_G5641)),
%         reflexiveObjectProperty(knows(_G5673,_G5676)),
%         irreflexiveObjectProperty(marriedTo(_G5720,_G5723)),
%         symmetric friend,
%         asymmetric parentOf,
%         transitive ancestorOf,
%         hasLastName subproperty hasName,
%         equivalentDataProperties(hasName(_G5924,_G5927),seLlama(_G5960,_G5963)),
%         hasName domain 'Person',
%         hasName range string,
%         functional hasAge].

% %DisjointObjectProperties(a:hasFather a:hasMother)
% %DisjointDataProperties(a:hasName a:hasAddress)
% test(unusual_axioms) :-
%     Input = "Declaration(Datatype(a:SSN))
% DatatypeDefinition(a:SSN DatatypeRestriction(xsd:string xsd:pattern \"[0-9]{3}-[0-9]{2}-[0-9]{4}\"))
% HasKey(owl:Thing () (a:hasSSN))
% ",
%     axioms(Output, Input, []).

% test(assertions) :- 
%     Input = "SameIndividual(a:Meg a:Megan)
% DifferentIndividuals(a:Peter a:Meg a:Chris a:Stewie)
% ClassAssertion(a:Dog a:Brian)
% ObjectPropertyAssertion(a:hasDog a:Peter a:Brian)
% NegativeObjectPropertyAssertion(a:hasSon a:Peter a:Meg)
% DataPropertyAssertion(a:hasAge a:Meg "17"^^xsd:integer)
% NegativeDataPropertyAssertion(a:hasAge a:Meg "5"^^xsd:integer)
% ",
%     axioms(Output, Input, []).

% test(annotations) :- 
%     Input = "AnnotationAssertion(rdfs:label a:Person \"Represents the set of all people.\")
% ",
%     axioms(Output, Input, []).

% :- end_tests(examples_from_specification).
:- begin_tests(basic).

test(uri1) :-
    Input  = "<http://oaei.ontologymatching.org/2009/benchmarks/228/onto.rdf#Book>",
    uri(Output, Input, []),
    Output = uri(url, '<http://oaei.ontologymatching.org/2009/benchmarks/228/onto.rdf#Book>'),
    uri_to_name(Output, Name, _),
    Name   = 'Book'.

test(uri2) :-
    Input  = "<bib:Author>",
    uri(Output, Input, []),
    Output = uri(url, '<bib:Author>').

test(uri3) :-
    Input  = "<f://m#AdministrativeArea>",
    uri(Output, Input, []),
    Output = uri(url, '<f://m#AdministrativeArea>'),
    uri_to_name(Output, Name, _),
    Name  = 'AdministrativeArea'.

test(uri4) :-
    Input  = "<file:/home/aurona/0AlleWerk/Navorsing/Ontologies/NAP/NAP#Foldable_Wheelchair>",
    uri(Output, Input, []),
    Output = uri(url, '<file:/home/aurona/0AlleWerk/Navorsing/Ontologies/NAP/NAP#Foldable_Wheelchair>'),
    uri_to_name(Output, Name, _),
    Name = 'Foldable_Wheelchair'.

test(uri5) :-
    Input  = "owl:Thing",
    uri(Output, Input, []),
    Output = uri(prefix, owl, 'Thing', 'owl:Thing').

test(uri6) :-
    Input  = "<http://172.16.83.69/univ-bench-dl.owl#BaseballLover>",
    uri(Output, Input, []),
    Output = uri(url, '<http://172.16.83.69/univ-bench-dl.owl#BaseballLover>').

test(uri7) :-
    Input  = "<http://172.16.83.69/univ-bench-dl.owl/BaseballLover>",
    uri(Output, Input, []),
    Output = uri(url, '<http://172.16.83.69/univ-bench-dl.owl/BaseballLover>'),
    uri_to_name(Output, Name, _),
    Name = 'BaseballLover'.

test(uri8) :-
    Input  = "<http://kb.phenoscape.org/uuid/cc266256-17ca-4cbf-8612-64d42c0032ad-8>",
    uri(Output, Input, []),
    Output = uri(url, '<http://kb.phenoscape.org/uuid/cc266256-17ca-4cbf-8612-64d42c0032ad-8>'),
    uri_to_name(Output, Name, _),
    Name = 'cc266256-17ca-4cbf-8612-64d42c0032ad-8'.

test(uri9) :-
    Input  = "<http://confOf#Social_event>",
    uri(Output, Input, []),
    Output = uri(url, '<http://confOf#Social_event>').

test(prefix) :-
    Input  = "Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)",
    prefix(Output, Input, []),
    Output == prefix(rdf, uri(url, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>')).

test(ontologyImport) :-
    Input  = "Import(<http://www.co-ode.org/ontologies/pizza/pizza.owl>)",
    import(Output, Input, []),
    Output == import(uri(url, '<http://www.co-ode.org/ontologies/pizza/pizza.owl>')).

test(uri1) :-
    Input = "ObjectPropertyAssertion(:hasBody :VentanaCheninBlanc :Medium)",
    Output = hasBody assert_p ['VentanaCheninBlanc', 'Medium'],
    axiom(Output, Input, []).

test(uri2) :-
    Input = ":atomic-number",
    uri(Output, Input, []),
    Output == uri(empty, 'atomic-number', ':atomic-number').

test(entity1) :-
    Input = "abc:atomic-number",
    entity(Output, Input, []),
    Output == 'atomic-number'.

test(individual) :-
    Input  = "Declaration(NamedIndividual(<http://www.cin.ufpe.br/~astm/granparent.owl#pc>))",
    Output = individual pc,
    declaration(Output, Input, []).

test(classDeclaration2) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))",
    declaration(Output, Input, []),
    Output = class ['Animal', '<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>'].

test(individual1) :-
    Input  = "ClassAssertion(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Degree> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#BA>)",
    Output = 'Degree' assert 'BA',
    classAssertion(Output, Input, []).

test(individual2) :- 
    Input  = "ClassAssertion(<http://purl.obolibrary.org/obo/BFO_0000053_some_http://purl.obolibrary.org/obo/PATO_0000467> <http://kb.phenoscape.org/uuid/cc266256-17ca-4cbf-8612-64d42c0032ad-10>)",
    classAssertion(Output, Input, []),
    Output = 'PATO_0000467' assert 'cc266256-17ca-4cbf-8612-64d42c0032ad-10'.

test(hasKey1) :-
    Input  = "HasKey(:Thing ( ) ())",
    Output = 'Thing' haskey [null, null],
    axiom(Output, Input, []).

test(hasKey2) :-
    Input  = "HasKey(:Thing (:MyProperty) ( ))",
    Output = 'Thing'haskey['MyProperty', null],
    axiom(Output, Input, []).

test(hasKey3) :-
    Input  = "HasKey(:Thing (:MyProperty) (:OtherProperty))",
    Output = 'Thing' haskey ['MyProperty', 'OtherProperty'],
    axiom(Output, Input, []).

test(propertyDomain) :-
    Input  = "ObjectPropertyDomain(<http://www.cin.ufpe.br/~astm/dataproperty#hasAbc> <http://www.cin.ufpe.br/~astm/dataproperty#C>)",
    Output = hasAbc domain 'C',
    axiom(Output, Input, []).

test(propertyRange1) :-
    Input  = "ObjectPropertyRange(<http://www.cin.ufpe.br/~astm/dataproperty#hasAbc> <http://www.cin.ufpe.br/~astm/dataproperty#C>)",
    Output = hasAbc range 'C',
    axiom(Output, Input, []).

test(propertyRange2) :-
    Input  = "ObjectPropertyRange(<http://www.cin.ufpe.br/~astm/dataproperty#hasAbc> ObjectSomeValuesFrom(<http://www.cin.ufpe.br/~astm/dataproperty#hasAbc> <http://www.cin.ufpe.br/~astm/dataproperty#C>))",
    Output = hasAbc range (hasAbc some 'C'),
    axiom(Output, Input, []).

test(inverseObjectProperty) :-
    Input  = "InverseObjectProperties(<http://www.cin.ufpe.br/~astm/dataproperty#hasCba> <http://www.cin.ufpe.br/~astm/dataproperty#hasAbc>)",
    Output = hasCba inverse hasAbc,
    axiom(Output, Input, []).

test(symmetricObjectProperty) :-
    Input  = "SymmetricObjectProperty(<http://www.cin.ufpe.br/~astm/dataproperty#hasCba>)",
    Output = symmetric hasCba,
    axiom(Output, Input, []).

test(propertyassertion1) :-
    Input  = "ObjectPropertyAssertion(<http://www.cin.ufpe.br/~astm/cycles1.owl#hasSon> <http://www.cin.ufpe.br/~astm/cycles1.owl#Luiz> <http://www.cin.ufpe.br/~astm/cycles1.owl#Fred>)",
    Output = hasSon assert_p ['Luiz', 'Fred'],
    assertion(Output, Input, []).

test(propertyassertion2) :-
    Input  = "DataPropertyAssertion(<http://www.cin.ufpe.br/~astm/dataproperty#temNome> <http://www.cin.ufpe.br/~astm/dataproperty#c> \"nomeC\")",
    Output = temNome assert_p [c, nomeC],
    assertion(Output, Input, []).

test(negativeObjectPropertyAssertion) :-
    Input = "NegativeObjectPropertyAssertion(a:hasSon a:Peter a:Meg)",
    assertion(Output, Input, []),
    Output = hasSon negative_p ['Peter', 'Meg'].

test(negativeDataPropertyAssertion) :-
    Input = "NegativeDataPropertyAssertion(a:hasAge a:Meg \"5\"^^xsd:integer)",
    assertion(Output, Input, []),
    Output = hasAge negative_p ['Meg', 5].

test(somevaluesfrom1) :-
    Input  = "ObjectSomeValuesFrom(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#hasHabitat> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#DryEucalyptForest>)",
    Output = hasHabitat some 'DryEucalyptForest',
    classExpression(Output, Input, []).

test(somevaluesfrom2) :-
    Input  = "ObjectSomeValuesFrom(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#hasHabitat> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB>))",
    Output = hasHabitat some ('ClassA' or 'ClassB'),
    classExpression(Output, Input, []).

test(datasomevaluesfrom1) :-
    Input  = "DataSomeValuesFrom(:atomic-number xsd:integer)",
    Output = 'atomic-number' some integer,
    classExpression(Output, Input, []).

test(allvaluesfrom) :-
    Input  = "ObjectAllValuesFrom(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#hasHabitat> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#DryEucalyptForest>)",
    Output = hasHabitat any 'DryEucalyptForest',
    classExpression(Output, Input, []).

test(intersection1) :-
    Input  = "ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB>)",
    Output = 'ClassA' and 'ClassB',
    classExpression(Output, Input, []).

test(intersection2) :-
    Input  = "ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassC>)",
    Output = 'ClassA' and ('ClassB' and 'ClassC'),
    classExpression(Output, Input, []).

test(intersection3) :-
    Input  = "ObjectIntersectionOf(ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>)",
    Output = ('hasPart' some 'Bone')and 'Animal',
    classExpression(Output, Input, []).

test(union1) :-
    Input  = "ObjectUnionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB>)",
    Output = 'ClassA' or 'ClassB',
    classExpression(Output, Input, []).

test(union2) :-
    Input  = "ObjectUnionOf(<http://www.cin.ufpe.br/~astm/owl/null.owl#ClassA> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassB> <http://www.cin.ufpe.br/~astm/owl/null.owl#ClassC>)",
    Output = 'ClassA' or ('ClassB' or 'ClassC'),
    classExpression(Output, Input, []).

test(union3) :-
    Input  = "ObjectUnionOf(ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>)",
    Output = ('hasPart' some 'Bone') or 'Animal',
    classExpression(Output, Input, []).
    
test(union4) :-
    Input  = "ObjectUnionOf(<http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor> <http://www.cin.ufpe.br/~astm/cycles1.owl#Dr>)",
    Output = 'DrAncestor' or 'Dr',
    classExpression(Output, Input, []).

test(maxCardinality1) :-
    Input = "ObjectMaxCardinality(1 :madeFromGrape)",
    Output = 1 max madeFromGrape,
    classExpression(Output, Input, []).

test(objectOneOf1) :-
    Input = "ObjectOneOf(:Peter :Lois :Stewie :Meg :Chris :Brian)",
    Output = 'Peter' one ('Lois' one ('Stewie' one ('Meg' one ('Chris' one 'Brian')))),
    classExpression(Output, Input, []).

test(dataHasValue1) :-
    Input = "DataHasValue(:dataproperty18 \"something\")",
    Output = dataproperty18 value something,
    classExpression(Output, Input, []).

test(subClass1) :-
    Input = "SubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)",
    Output = 'TasmanianDevil' is_a 'Marsupials',
    axiom(Output, Input, []).

test(subClass2) :-
    Input = "SubClassOf(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Bird> ObjectIntersectionOf(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal> ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Feather>)))",
    Output = 'Bird' is_a ('Animal' and (('hasPart' some 'Bone') and ('hasPart' some 'Feather'))),
    axiom(Output, Input, []).

test(subClass3) :-
    Input  = "SubClassOf(ObjectIntersectionOf(ObjectSomeValuesFrom(<http://www.co-ode.org/ontologies/ont.owl#hasPart> <http://www.cin.ufpe.br/~astm/owl/bird.owl#Bone>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>) <http://www.cin.ufpe.br/~astm/owl/bird.owl#Vertebrate>)",
    Output = (('hasPart' some 'Bone') and 'Animal') is_a 'Vertebrate',
    axiom(Output, Input, []).

test(gci1) :-
    Input = "SubClassOf(ObjectSomeValuesFrom(<http://www.cin.ufpe.br/~astm/cycles1.owl#hasSon> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor> <http://www.cin.ufpe.br/~astm/cycles1.owl#Dr>)) <http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor>)",
    Output = ('hasSon' some ('DrAncestor' or 'Dr')) is_a 'DrAncestor',
    axiom(Output, Input, []).

test(gci2) :-
    Input = "SubClassOf(ObjectUnionOf(<http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor> <http://www.cin.ufpe.br/~astm/cycles1.owl#Dr>) <http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor>)",
    Output = ('DrAncestor' or 'Dr') is_a 'DrAncestor',
    axiom(Output, Input, []).

test(gci3) :-
    Input = "SubClassOf(ObjectSomeValuesFrom(<http://www.cin.ufpe.br/~astm/cycles1.owl#hasSon> <http://www.cin.ufpe.br/~astm/cycles1.owl#Dr>) <http://www.cin.ufpe.br/~astm/cycles1.owl#DrAncestor>)",
    Output = ('hasSon' some 'Dr') is_a 'DrAncestor',
    axiom(Output, Input, []).

test(equivalent1) :-
    Input = "EquivalentClasses(<http://www.cin.ufpe.br/~astm/wine.owl#WineDescriptor> ObjectUnionOf(<http://www.cin.ufpe.br/~astm/wine.owl#WineTaste> <http://www.cin.ufpe.br/~astm/wine.owl#WineColor>))",
    Output = 'WineDescriptor' equivalent ('WineTaste' or 'WineColor'),
    axiom(Output, Input, []).

test(equivalent2) :-
    Input = "EquivalentClasses(:AlsatianWine ObjectIntersectionOf(ObjectHasValue(:locatedIn :AlsaceRegion) :Wine))",
    Output = 'AlsatianWine' equivalent ((locatedIn value 'AlsaceRegion') and'Wine'),
    axiom(Output, Input, []).

test(equivalent3) :-
    Input  = "EquivalentClasses(:Meritage ObjectIntersectionOf(:Wine ObjectAllValuesFrom(:madeFromGrape ObjectOneOf(:PetiteVerdotGrape :MerlotGrape :MalbecGrape :CabernetSauvignonGrape :CabernetFrancGrape)) ObjectMinCardinality(2 :madeFromGrape)))",
    Output = 'Meritage' equivalent ('Wine' and ((madeFromGrape any ('PetiteVerdotGrape' one ('MerlotGrape' one ('MalbecGrape'one ('CabernetSauvignonGrape' one 'CabernetFrancGrape'))))) and (2 min madeFromGrape))),
    axiom(Output, Input, []).

test(equivalent4) :-
    Input = "EquivalentClasses(:A ObjectIntersectionOf(:C :G ObjectUnionOf(:D :E :F) ObjectUnionOf(:I :H)))",
    Output = 'A' equivalent ('C'and ('G'and (('D' or ('E' or 'F')) and ('I' or 'H')))),
    axiom(Output, Input, []).

test(equivalentObjectProperties1) :-
    Input = "EquivalentObjectProperties(<http://www.cin.ufpe.br/~astm/wine.owl#PropertyA> <http://www.cin.ufpe.br/~astm/wine.owl#PropertyB>)",
    Output = 'PropertyA' equivalent_p 'PropertyB',
    axiom(Output, Input, []).

test(equivalentDataProperties1) :-
    Input = "EquivalentDataProperties(<http://www.cin.ufpe.br/~astm/wine.owl#PropertyA> <http://www.cin.ufpe.br/~astm/wine.owl#PropertyB>)",
    Output = 'PropertyA' equivalent_p 'PropertyB',
    axiom(Output, Input, []).

test(disjoint) :-
    Input  = "DisjointClasses(<http://www.cin.ufpe.br/~astm/owl/bird.owl#A> <http://www.cin.ufpe.br/~astm/owl/bird.owl#B>)",
    Output = 'A' disjoint 'B',
    axiom(Output, Input, []).

test(disjoint2) :-
    Input  = "DisjointClasses(<http://www.cin.ufpe.br/~astm/owl/bird.owl#A> <http://www.cin.ufpe.br/~astm/owl/bird.owl#B> <http://www.cin.ufpe.br/~astm/owl/bird.owl#C>)",
    axiom(Output, Input, []),
    Output == 'A' disjoint ('B' disjoint 'C').

test(subObjectPropertyOf1) :-
    Input  = "SubObjectPropertyOf(:hasBody :hasWineDescriptor)",
    Output = 'hasBody' subproperty 'hasWineDescriptor',
    axiom(Output, Input, []).

test(differentIndividuals1) :- 
    Input  = "DifferentIndividuals(:Delicate :Moderate :Strong)",
    Output = 'Delicate' different ('Moderate' different 'Strong'),
    axiom(Output, Input, []).

test(sameIndividuals1) :- 
    Input  = "SameIndividual(:Delicate :Moderate :Strong)",
    Output = 'Delicate' same ('Moderate' same 'Strong'),
    axiom(Output, Input, []).

:- end_tests(basic).
:- begin_tests(combined).

test(prefixes1) :-
    Input = "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\nPrefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n",
    prefixes(Prefixes, Input, _),
    Prefixes == [prefix(owl, uri(url, '<http://www.w3.org/2002/07/owl#>')), prefix(rdfs, uri(url, '<http://www.w3.org/2000/01/rdf-schema#>'))].

test(ontology1) :-
    Input = "Ontology(<http://www.cin.ufpe.br/~astm/or.owl>\n\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#A>))\n)",
    ontology(Imports, Axioms, Input, []),
    Imports = [],
    Axioms = [class['A', '<http://www.cin.ufpe.br/~astm/or.owl#A>']].

test(ontology2) :-
    Input = "Ontology(<http://cin.ufpe.br/~astm/owl/bird.owl>\nSubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)\n)",
    Imports = [],
    Axioms = ['TasmanianDevil' is_a 'Marsupials'],
    ontology(Imports, Axioms, Input, []).

test(owl1) :-
    Input = "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\n\n\nOntology(<http://www.cin.ufpe.br/~astm/or.owl>\n\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/or.owl#A>))\n)",
    owl(Prefixes, Imports, Axioms, Input, []),
    Prefixes == [prefix(owl, uri(url, '<http://www.w3.org/2002/07/owl#>'))],
    Imports == [],
    Axioms = [class['A', '<http://www.cin.ufpe.br/~astm/or.owl#A>']].

test(axioms1) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\n",
    axioms(Output, Input, []),
    Output = [class ['Animal', '<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>']].

test(axioms2) :-
    Input  = "SubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)\nSubClassOf(<http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#TasmanianDevil> <http://protege.stanford.edu/plugins/owl/owl-library/koala.owl#Marsupials>)\n",
    axioms(Output, Input, []),
    Output = ['TasmanianDevil' is_a 'Marsupials', 'TasmanianDevil' is_a 'Marsupials'].

test(axioms3) :-
    Input  = "Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\nDeclaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))\n",
    axioms(Output, Input, []),
    Output = [class['Animal', '<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>'], class['Animal', '<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>'], class['Animal', '<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>']].

:- end_tests(combined).
:- run_tests.

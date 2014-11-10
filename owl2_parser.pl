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

%    Parser for OWL 2 (Web Ontology Language)
%    Functional-Style Syntax (Second Edition)
%    http://www.w3.org/TR/owl2-syntax/

:- [owl2_operators].

owl(Prefixes, Imports, Axioms) --> 
    prefixes(Prefixes), newline, ontology(Imports, Axioms).

prefix(prefix(Ns, Uri)) -->
    "Prefix(", word(Ns) ,":=", uri(Uri), ")".

ontology(Imports, Axioms) --> 
    "Ontology(", uri(_), newline, imports(Imports), axioms(Axioms), ")", newline, !.

import(import(Uri)) -->
    "Import(", uri(Uri), ")".

declarationClass(class [Class, Uri]) -->
    "Declaration(Class(", uri(URI), "))", { uri_to_name(URI, Class, Uri) }.

declarationNamedIndividual((individual Name)) -->
    "Declaration(NamedIndividual(", entity(Name), "))".

declarationObjectProperty((property Property)) -->
    "Declaration(ObjectProperty(", entity(Property), "))".

declarationDataProperty((property Property)) -->
    "Declaration(DataProperty(", entity(Property), "))".

classAssertion((ClassName assert InstanceName)) --> 
    "ClassAssertion(", entity(ClassName), " ", entity(InstanceName), ")".

objectPropertyDomain((PropertyName domain Exp)) -->
    "ObjectPropertyDomain(", entity(PropertyName), " ", classExpression(Exp), ")".

objectPropertyRange((PropertyName range Exp)) -->
    "ObjectPropertyRange(", entity(PropertyName), " ", classExpression(Exp), ")".

objectPropertyAssertion((PropertyName assert_p [Ind1, Ind2])) --> 
    "ObjectPropertyAssertion(", entity(PropertyName), " ", entity(Ind1), " ", entity(Ind2), ")".

dataPropertyDomain((PropertyName domain Exp)) -->
    "DataPropertyDomain(", entity(PropertyName), " ", classExpression(Exp), ")".

dataPropertyRange((PropertyName range Exp)) -->
    "DataPropertyRange(", entity(PropertyName), " ", classExpression(Exp), ")".

dataPropertyAssertion((PropetyName assert_p [Ind1, Ind2])) --> 
    "DataPropertyAssertion(", entity(PropetyName), " ", entity(Ind1), " ", any_chars(Chars), ")",
        { name(Ind2, Chars), ! }.

inverseObjectProperties((PropertyA inverse PropertyB)) -->
    "InverseObjectProperties(", entity(PropertyA), " ", entity(PropertyB), ")".

symmetricObjectProperty((symmetric PropertyName)) -->
    "SymmetricObjectProperty(", entity(PropertyName), ")".

asymmetricObjectProperty((asymmetric PropertyName)) -->
    "AsymmetricObjectProperty(", entity(PropertyName), ")".

reflexiveObjectProperty((reflexive Property)) -->
    "ReflexiveObjectProperty(", entity(Property), ")".

irreflexiveObjectProperty((irreflexive Property)) -->
    "IrreflexiveObjectProperty(", entity(Property), ")".

transitiveObjectProperty((transitive PropertyName)) -->
    "TransitiveObjectProperty(", entity(PropertyName), ")".

functionalObjectProperty((functional PropertyName)) -->
    "FunctionalObjectProperty(", entity(PropertyName), ")".

functionalDataProperty((functional PropertyName)) -->
    "FunctionalDataProperty(", entity(PropertyName), ")".

inverseFunctionalObjectProperty((inverse_functional PropertyName)) -->
    "InverseFunctionalObjectProperty(", entity(PropertyName), ")".

negativeObjectPropertyAssertion((PropertyName negative_p [Ind1, Ind2])) -->
    "NegativeObjectPropertyAssertion(", entity(PropertyName), " ", entity(Ind1), " ", entity(Ind2), ")".

negativeDataPropertyAssertion((PropertyName negative_p [Ind1, Ind2])) -->
    "NegativeDataPropertyAssertion(", entity(PropertyName), " ", entity(Ind1), " ", literal(Ind2), ")".

objectSomeValuesFrom((PropertyName some Expression)) -->
    "ObjectSomeValuesFrom(", entity(PropertyName), " ", classExpression(Expression), ")".

dataSomeValuesFrom((PropertyName some Type)) -->
    "DataSomeValuesFrom(", entity(PropertyName), " ", classExpression(Type), ")".

objectAllValuesFrom((PropertyName any Expression)) -->
    "ObjectAllValuesFrom(", entity(PropertyName), " ", classExpression(Expression), ")".

dataAllValuesFrom((PropertyName any Expression)) -->
    "DataAllValuesFrom(", entity(PropertyName), " ", classExpression(Expression), ")".

objectUnionOf(Union) --> 
    "ObjectUnionOf(", objectUnionOfExpression(Union), ")".

objectIntersectionOf(Intersection) --> 
    "ObjectIntersectionOf(", objectIntersectionOfExpression(Intersection), ")".

dataIntersectionOf(Intersection) --> 
    "DataIntersectionOf(", dataIntersectionOfExpression(Intersection), ")".

objectMaxCardinality(([Number, PropertyName] max Expression)) --> 
    "ObjectMaxCardinality(", word(NumberValue), " ", entity(PropertyName), " ", classExpression(Expression), ")",
        { atom_number(NumberValue, Number), ! }.

objectMaxCardinality((Number max PropertyName)) --> 
    "ObjectMaxCardinality(", word(NumberValue), " ", entity(PropertyName), ")",
        { atom_number(NumberValue, Number) }.

dataMaxCardinality(([Number, PropertyName] max_d Expression)) --> 
    "DataMaxCardinality(", word(NumberValue), " ", entity(PropertyName), " ", classExpression(Expression), ")",
        { atom_number(NumberValue, Number), ! }.

dataMaxCardinality((Number max_d PropertyName)) --> 
    "DataMaxCardinality(", word(NumberValue), " ", entity(PropertyName), ")",
        { atom_number(NumberValue, Number) }.

objectMinCardinality(([Number, PropertyName] min Expression)) --> 
    "ObjectMinCardinality(", word(NumberValue), " ", entity(PropertyName), " ", classExpression(Expression), ")",
        { atom_number(NumberValue, Number), ! }.

objectMinCardinality((Number min PropertyName)) --> 
    "ObjectMinCardinality(", word(NumberValue), " ", entity(PropertyName), ")",
        { atom_number(NumberValue, Number) }.

dataMinCardinality(([Number, PropertyName] min_d Expression)) --> 
    "DataMinCardinality(", word(NumberValue), " ", entity(PropertyName), " ", classExpression(Expression), ")",
        { atom_number(NumberValue, Number), ! }.

dataMinCardinality((Number min_d PropertyName)) --> 
    "DataMinCardinality(", word(NumberValue), " ", entity(PropertyName), ")",
        { atom_number(NumberValue, Number) }.

objectExactCardinality(([Number, PropertyName] exact Expression)) --> 
    "ObjectExactCardinality(", word(NumberValue), " ", entity(PropertyName), " ", classExpression(Expression), ")",
        { atom_number(NumberValue, Number), ! }.

objectExactCardinality((Number exact PropertyName)) --> 
    "ObjectExactCardinality(", word(NumberValue), " ", entity(PropertyName), ")",
        { atom_number(NumberValue, Number) }.

dataExactCardinality(([Number, PropertyName] exact_d Expression)) --> 
    "DataExactCardinality(", word(NumberValue), " ", entity(PropertyName), " ", classExpression(Expression), ")",
        { atom_number(NumberValue, Number), ! }.

dataExactCardinality((Number exact_d PropertyName)) --> 
    "DataExactCardinality(", word(NumberValue), " ", entity(PropertyName), ")",
        { atom_number(NumberValue, Number) }.

objectHasValue((PropertyName value IndividualName)) --> 
    "ObjectHasValue(", entity(PropertyName), " ", entity(IndividualName), ")".

dataHasValue((PropertyName value LiteralName)) --> 
    "DataHasValue(", entity(PropertyName), " ", literal(LiteralName), ")".

objectHasSelf((self PropertyName)) --> 
    "ObjectHasSelf(", entity(PropertyName), ")".

objectComplementOf((not Exp)) -->
    "ObjectComplementOf(", classExpression(Exp), ")".

subObjectPropertyOf((PropA subproperty PropB)) --> 
    "SubObjectPropertyOf(", entity(PropA), " ", entity(PropB), ")".

subDataPropertyOf((PropA subproperty PropB)) --> 
    "SubDataPropertyOf(", entity(PropA), " ", entity(PropB), ")".

hasKey((ClassExpression haskey [ObjectProperty, DataProperty])) -->
    "HasKey(", entity(ClassExpression), " (", property_or_null(ObjectProperty), ") (", property_or_null(DataProperty), "))".

objectOneOf(OneOf) -->
    "ObjectOneOf(", objectOneOfExpression(OneOf), ")".

subClassOf((Exp1 is_a Exp2)) --> 
    "SubClassOf(", classExpression(Exp1), " ", classExpression(Exp2), ")".

equivalentClasses((Exp1 equivalent Exp2)) -->
    "EquivalentClasses(", classExpression(Exp1), " ", classExpression(Exp2), ")".

equivalentDataProperties(Equivalent) -->
    "EquivalentDataProperties(", equivalentDataPropertiesExpression(Equivalent), ")".

equivalentObjectProperties(Equivalent) -->
    "EquivalentObjectProperties(", equivalentObjectPropertiesExpression(Equivalent), ")".

disjointClasses(Disjoint) -->
    "DisjointClasses(", disjointExpression(Disjoint), ")".

disjointUnion(Disjoint) -->
    "DisjointUnion(", disjointUnionExpression(Disjoint), ")".

differentIndividuals(Different) -->
    "DifferentIndividuals(", differentIndividualsExpression(Different), ")".

sameIndividual(SameIndividual) -->
    "SameIndividual(", sameIndividualExpression(SameIndividual), ")".

entity(Name) -->
    "<", any_chars(_), "#", word(Name), ">", { ! }.

entity(Name) -->
    class_name_chars(_), ":", class_name_chars(Chars), { name(Name, Chars), ! }.

entity(Name) -->
    ":", class_name_chars(Chars), { name(Name, Chars), ! }.

uri(uri(url, Uri)) -->
    "<", any_chars(Chars), ">", { name(Uri1, Chars), atomic_list_concat(['<', Uri1, '>'], Uri), ! }.

uri(uri(empty, Name, Uri)) -->
    ":", any_chars(Chars), { name(Name, Chars), atom_codes(Uri, [58 | Chars]), ! }.

uri(uri(prefix, Prefix, Name, Uri)) -->
    class_name_chars(Uri1), ":", class_name_chars(Uri2),
        { name(Prefix, Uri1), name(Name, Uri2), atomic_list_concat([Prefix, ':', Name], Uri), ! }.

dataRange(Name) -->
    entity(Name).

literal(Name) -->
    quotedString(Name).

quotedString(Name) --> 
    "\"", any_chars(Chars), "\"", { name(Name, Chars), ! }.

property_or_null(Property) --> entity(Property), { ! }.
property_or_null(null) --> space.

%%%%%%%%%%%%%%%%%%%%%%
% Complement Clauses %
%%%%%%%%%%%%%%%%%%%%%%

property(Class) --> entity(PropertyName), { Class=..[PropertyName,_,_] }.
class(Name) --> uri(URI), { uri_to_name(URI, Name, _) }. %, { Class=..[ClassName,_] }.

declaration(Exp) --> declarationClass(Exp), !.
%declaration(Exp) --> declarationDatatype(Exp), !.
declaration(Exp) --> declarationObjectProperty(Exp), !.
declaration(Exp) --> declarationDataProperty(Exp), !.
%declaration(Exp) --> declarationAnnotationProperty(Exp), !.
declaration(Exp) --> declarationNamedIndividual(Exp).

assertion(X) --> sameIndividual(X), !.
assertion(X) --> differentIndividuals(X), !.
assertion(X) --> classAssertion(X), !.
assertion(X) --> objectPropertyAssertion(X), !.
assertion(X) --> negativeObjectPropertyAssertion(X), !.
assertion(X) --> negativeDataPropertyAssertion(X), !.
assertion(X) --> dataPropertyAssertion(X).

classExpression(Exp) --> objectIntersectionOf(Exp), !.
classExpression(Exp) --> objectUnionOf(Exp), !.
classExpression(Exp) --> objectComplementOf(Exp), !.
classExpression(Exp) --> objectOneOf(Exp), !.
classExpression(Exp) --> objectSomeValuesFrom(Exp), !.
classExpression(Exp) --> objectAllValuesFrom(Exp), !.
classExpression(Exp) --> objectHasValue(Exp), !.
classExpression(Exp) --> objectHasSelf(Exp).
classExpression(Exp) --> objectMinCardinality(Exp), !.
classExpression(Exp) --> objectMaxCardinality(Exp), !.
classExpression(Exp) --> objectExactCardinality(Exp), !.
classExpression(Exp) --> dataIntersectionOf(Exp), !.
classExpression(Exp) --> dataSomeValuesFrom(Exp), !.
classExpression(Exp) --> dataAllValuesFrom(Exp).
classExpression(Exp) --> dataHasValue(Exp), !.
classExpression(Exp) --> dataMinCardinality(Exp).
classExpression(Exp) --> dataMaxCardinality(Exp).
classExpression(Exp) --> dataExactCardinality(Exp).
classExpression(Exp) --> class(Exp).

objectPropertyAxiom(Ax) --> subObjectPropertyOf(Ax), !.
objectPropertyAxiom(Ax) --> equivalentObjectProperties(Ax), !.
%objectPropertyAxiom(Ax) --> disjointObjectProperties(Ax), !.
objectPropertyAxiom(Ax) --> inverseObjectProperties(Ax), !.
objectPropertyAxiom(Ax) --> objectPropertyDomain(Ax), !.
objectPropertyAxiom(Ax) --> objectPropertyRange(Ax), !.
objectPropertyAxiom(Ax) --> functionalObjectProperty(Ax), !.
objectPropertyAxiom(Ax) --> functionalDataProperty(Ax), !.
objectPropertyAxiom(Ax) --> inverseFunctionalObjectProperty(Ax), !.
objectPropertyAxiom(Ax) --> reflexiveObjectProperty(Ax), !.
objectPropertyAxiom(Ax) --> irreflexiveObjectProperty(Ax), !.
objectPropertyAxiom(Ax) --> symmetricObjectProperty(Ax), !.
objectPropertyAxiom(Ax) --> asymmetricObjectProperty(Ax), !.
objectPropertyAxiom(Ax) --> transitiveObjectProperty(Ax), !.

dataPropertyAxiom(Ax) --> subDataPropertyOf(Ax), !.
dataPropertyAxiom(Ax) --> equivalentDataProperties(Ax), !.
%dataPropertyAxiom(Ax) --> disjointDataProperties(Ax), !.
dataPropertyAxiom(Ax) --> dataPropertyDomain(Ax), !.
dataPropertyAxiom(Ax) --> dataPropertyRange(Ax).
%dataPropertyAxiom(Ax) --> functionalDataProperty(Ax), !.

axiom(X) --> declaration(X), !.
axiom(X) --> classAxiom(X), !.
axiom(X) --> objectPropertyAxiom(X), !.
axiom(X) --> dataPropertyAxiom(X), !.
%axiom(X) --> dataTypeDefinition(X), !.
axiom(X) --> hasKey(X), !.
axiom(X) --> assertion(X).
%axiom(X) --> annotationAxiom(X).

classAxiom(X) --> subClassOf(X), !.
classAxiom(X) --> equivalentClasses(X), !.
classAxiom(X) --> disjointClasses(X).
classAxiom(X) --> disjointUnion(X), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Clauses that create lists %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imports([Import|Imports]) --> import(Import), "\n", imports(Imports), !.
imports([]) --> [].

prefixes([Prefix|Prefixes]) --> prefix(Prefix), "\n", prefixes(Prefixes), !.
prefixes([]) --> [].

axioms([Axiom|Axioms]) --> axiom(Axiom), "\n", axioms(Axioms), !.
axioms([]) --> [].

objectUnionOfExpression((Exp1 or Exp2)) --> classExpression(Exp1), " ", objectUnionOfExpression(Exp2), !.
objectUnionOfExpression(Expression) --> classExpression(Expression).

objectIntersectionOfExpression((Exp1 and Exp2)) --> classExpression(Exp1), " ", objectIntersectionOfExpression(Exp2), !.
objectIntersectionOfExpression(Expression) --> classExpression(Expression).

dataIntersectionOfExpression((Exp1 and Exp2)) --> dataRange(Exp1), " ", dataIntersectionOfExpression(Exp2), !.
dataIntersectionOfExpression(Expression) --> dataRange(Expression).

objectOneOfExpression((Exp1 one Exp2)) --> entity(Exp1), " ", objectOneOfExpression(Exp2), !.
objectOneOfExpression(Expression) --> entity(Expression).

equivalentObjectPropertiesExpression((Exp1 equivalent_p Exp2)) --> entity(Exp1), " ", equivalentObjectPropertiesExpression(Exp2), !.
equivalentObjectPropertiesExpression(Expression) --> entity(Expression).

equivalentDataPropertiesExpression((Exp1 equivalent_p Exp2)) --> entity(Exp1), " ", equivalentDataPropertiesExpression(Exp2), !.
equivalentDataPropertiesExpression(Expression) --> entity(Expression).

disjointExpression((Exp1 disjoint Exp2)) --> class(Exp1), " ", disjointExpression(Exp2), !.
disjointExpression(Expression) --> class(Expression).

disjointUnionExpression((Exp1 disjoint_union Exp2)) --> class(Exp1), " ", disjointUnionExpression(Exp2), !.
disjointUnionExpression(Expression) --> class(Expression).

differentIndividualsExpression((Ind1 different Ind2)) --> entity(Ind1), " ", differentIndividualsExpression(Ind2), !.
differentIndividualsExpression(Individual) --> entity(Individual).

sameIndividualExpression((Ind1 same Ind2)) --> entity(Ind1), " ", sameIndividualExpression(Ind2), !.
sameIndividualExpression(Individual) --> entity(Individual).

%%%%%%%%%%%%%%%%%
%% Helper Rules %
%%%%%%%%%%%%%%%%%

% 32: ' ', 41: ), 10: \n, 58: ':'

is_any_char(X) :- X >= 0, X < 255, X \== 32, X \== 41, X \== 10.
is_class_name_char(X) :- X >= 0, X < 255, X \== 32, X \== 41, X \== 10, X \== 58.

uri_to_name(uri(empty, Name, Uri), Name, Uri).
uri_to_name(uri(prefix, _, Name, Uri), Name, Uri).
uri_to_name(uri(url, Uri), Name, Uri) :-                    % <http://oaei.ontologymatching.org/2009/benchmarks/228/onto.rdf#Book>
    atomic_list_concat([_, Name1], '#', Uri),
    atomic_list_concat([Name, _], '>', Name1), !.
uri_to_name(uri(url, Uri), Name, Uri) :-                    % <http://oaei.ontologymatching.org/2009/benchmarks/228/onto.rdf/Book>
    atomic_list_concat(List, '/', Uri),
    append(_, [Name1], List),
    atomic_list_concat([Name, _], '>', Name1), !.
uri_to_name(uri(url, Uri), Uri, Uri).

parse_owl(File, Prefixes, Imports, Axioms) :-
    read_file_to_codes(File, Input, []),
    owl(Prefixes, Imports, Axioms, Input, _).

%%%%%%%%%%%%%%%%%%%
%% Helper Clauses %
%%%%%%%%%%%%%%%%%%%

newline --> "\n", newline, !.
newline --> [].

space --> " ", space, !.
space --> [].

class_name_chars([X|Y]) --> class_name_char(X), class_name_chars(Y).
class_name_chars([]) --> [].

class_name_char(X) --> [X], { is_class_name_char(X) }.

any_chars([X|Y]) --> any_char(X), any_chars(Y).
any_chars([]) --> [].

any_char(X) --> [X], { is_any_char(X) }.

word(Word) --> chars(CHARS),
    { atom_codes(Word, CHARS), !  }.

chars([X|Y]) --> char(X), chars(Y).
chars([]) --> [].

char(X) --> [X], { is_char(X) }.

is_char(X) :- X >= 0'a, X =< 0'z, !.
is_char(X) :- X >= 0'A, X =< 0'Z, !.
is_char(X) :- X >= 0'0, X =< 0'9, !.
is_char(0'_).

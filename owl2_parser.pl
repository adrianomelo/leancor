
owl(Prefixes, Imports, Axioms) --> 
    prefixes(Prefixes), newline, ontology(Imports, Axioms).

prefix(prefix(Ns, Uri)) -->
    "Prefix(", word(Ns) ,":=", uri(Uri), ")".

ontology(Imports, Axioms) --> 
    "Ontology(", uri(_), newline, imports(Imports), axioms(Axioms), ")", !.

import(import(Uri)) -->
    "Import(", uri(Uri), ")".

declaration(class(Class)) -->
    "Declaration(Class(", class(Class), "))".

declaration(namedIndividual(Name)) -->
    "Declaration(NamedIndividual(", entity(Name), "))".

declaration(objectProperty(Property)) -->
    "Declaration(ObjectProperty(", property(Property), "))".

classAssertion(classAssertion(Instance)) --> 
    "ClassAssertion(", entity(ClassName), " ", entity(InstanceName), ")", 
        { Instance=..[ClassName, InstanceName] }.

objectPropertyDomain(objectPropertyDomain(Property, Exp)) -->
    "ObjectPropertyDomain(", property(Property), " ", classExpression(Exp), ")".

objectPropertyRange(objectPropertyRange(Property, Exp)) -->
    "ObjectPropertyRange(", property(Property), " ", classExpression(Exp), ")".

objectPropertyAssertion(objectPropertyAssertion(Property)) --> 
    "ObjectPropertyAssertion(", entity(PropName), " ", entity(Ind1), " ", entity(Ind2), ")",
        { Property=..[PropName, Ind1, Ind2] }.

dataPropertyDomain(dataPropertyDomain(Property, Exp)) -->
    "DataPropertyDomain(", property(Property), " ", classExpression(Exp), ")".

dataPropertyRange(dataPropertyRange(Property, Exp)) -->
    "DataPropertyRange(", property(Property), " ", classExpression(Exp), ")".

dataPropertyAssertion(dataPropertyAssertion(Property)) --> 
    "DataPropertyAssertion(", entity(PropName), " ", entity(Ind1), " ", any_chars(Chars), ")",
        { name(Ind2, Chars), Property=..[PropName, Ind1, Ind2], ! }.

inverseObjectProperties(inverseObjectProperties(PropertyA, PropertyB)) -->
    "InverseObjectProperties(", property(PropertyA), " ", property(PropertyB), ")".

symmetricObjectProperty(symmetricObjectProperty(Property)) -->
    "SymmetricObjectProperty(", property(Property), ")".

asymmetricObjectProperty(asymmetricObjectProperty(Property)) -->
    "AsymmetricObjectProperty(", property(Property), ")".

reflexiveObjectProperty(reflexiveObjectProperty(Property)) -->
    "ReflexiveObjectProperty(", property(Property), ")".

irreflexiveObjectProperty(irreflexiveObjectProperty(Property)) -->
    "IrreflexiveObjectProperty(", property(Property), ")".

transitiveObjectProperty(transitiveObjectProperty(Property)) -->
    "TransitiveObjectProperty(", property(Property), ")".

functionalObjectProperty(functionalObjectProperty(Property)) -->
    "FunctionalObjectProperty(", property(Property), ")".

inverseFunctionalObjectProperty(inverseFunctionalObjectProperty(Property)) -->
    "InverseFunctionalObjectProperty(", property(Property), ")".

objectSomeValuesFrom(objectSomeValuesFrom(Property, Expression)) -->
    "ObjectSomeValuesFrom(", property(Property), " ", classExpression(Expression), ")".

objectAllValuesFrom(objectAllValuesFrom(Property, Expression)) -->
    "ObjectAllValuesFrom(", property(Property), " ", classExpression(Expression), ")".

objectUnionOf(Union) --> 
    "ObjectUnionOf(", objectUnionOfExpression(Union), ")".

objectIntersectionOf(Intersection) --> 
    "ObjectIntersectionOf(", objectIntersectionOfExpression(Intersection), ")".

subClassOf(subClassOf(Exp1, Exp2)) --> 
    "SubClassOf(", classExpression(Exp1), " ", classExpression(Exp2), ")".

equivalentClasses(equivalentClasses(Exp1, Exp2)) -->
    "EquivalentClasses(", classExpression(Exp1), " ", classExpression(Exp2), ")".

disjoint(Disjoint) -->
    "DisjointClasses(", disjointExpression(Disjoint), ")".

entity(Name) -->
    "<", any_chars(_), "#", word(Name), ">", { ! }.

entity(Uri) -->
    ":", any_chars(Chars), { name(Name, Chars), downcase_atom(Name, Uri) }.

uri(Uri) --> ":", any_chars(Chars), { name(Uri, Chars), ! }.
uri(Uri) --> "<", any_chars(Chars), "#>", { name(Uri, Chars), ! }.
uri(Uri) --> "<", any_chars(Chars), ">", { name(Uri, Chars) }.

%%%%%%%%%%%%%%%%%%%%%%
% Complement Clauses %
%%%%%%%%%%%%%%%%%%%%%%

property(Class) --> entity(PropertyName), { Class=..[PropertyName,_,_] }.
class(Class) --> entity(ClassName), { Class=..[ClassName,_] }.

assertion(X) --> classAssertion(X), !.
assertion(X) --> objectPropertyAssertion(X), !.
assertion(X) --> dataPropertyAssertion(X).

classExpression(Exp) --> class(Exp), !.
classExpression(Exp) --> objectSomeValuesFrom(Exp), !.
classExpression(Exp) --> objectAllValuesFrom(Exp), !.
classExpression(Exp) --> objectUnionOf(Exp), !.
classExpression(Exp) --> objectIntersectionOf(Exp).
%classExpression(Exp) --> objectComplementOf(Exp).
%classExpression(Exp) --> objectOneOf(Exp).
%classExpression(Exp) --> objectObjectHasValue(Exp).
%classExpression(Exp) --> objectHasValue(Exp).
%classExpression(Exp) --> objectHasSelf(Exp).
%classExpression(Exp) --> objectMinCardinality(Exp).
%classExpression(Exp) --> objectMaxCardinality(Exp).
%classExpression(Exp) --> objectExactCardinality(Exp).
%classExpression(Exp) --> dataSomeValuesFrom(Exp).
%classExpression(Exp) --> dataAllValuesFrom(Exp).
%classExpression(Exp) --> dataHasValue(Exp).
%classExpression(Exp) --> dataMinCardinality(Exp).
%classExpression(Exp) --> dataMaxCardinality(Exp).
%classExpression(Exp) --> dataExactCardinality(Exp).

propertyProperties(Pro) --> objectPropertyDomain(Pro), !.
propertyProperties(Pro) --> objectPropertyRange(Pro), !.
propertyProperties(Pro) --> dataPropertyDomain(Pro), !.
propertyProperties(Pro) --> dataPropertyRange(Pro), !.
propertyProperties(Pro) --> symmetricObjectProperty(Pro), !.
propertyProperties(Pro) --> asymmetricObjectProperty(Pro), !.
propertyProperties(Pro) --> reflexiveObjectProperty(Pro), !.
propertyProperties(Pro) --> irreflexiveObjectProperty(Pro), !.
propertyProperties(Pro) --> transitiveObjectProperty(Pro), !.
propertyProperties(Pro) --> functionalObjectProperty(Pro), !.
propertyProperties(Pro) --> inverseFunctionalObjectProperty(Pro), !.
propertyProperties(Pro) --> inverseObjectProperties(Pro).

%axiom(X) --> classAxiom(X), !.
%axiom(X) --> declaration(X), !.
%axiom(X) --> objectPropertyAxiom(X), !.
%axiom(X) --> dataPropertyAxiom(X), !.
%axiom(X) --> dataTypeDefinition(X), !.
%axiom(X) --> hasKey(X), !.
%axiom(X) --> assetion(X), !.
%axiom(X) --> annotationAxiom(X).

%classAxiom(X) --> subClassOf(X), !.
%classAxiom(X) --> equivalentClasses(X), !.
%classAxiom(X) --> disjointClasses(X), !.
%classAxiom(X) --> disjointUnion(X), !.

axiom(X) --> subClassOf(X), !.
axiom(X) --> equivalentClasses(X), !.
axiom(X) --> declaration(X), !.
axiom(X) --> assertion(X), !.
axiom(X) --> propertyProperties(X), !.
axiom(X) --> disjoint(X).

imports([Import|Imports]) --> import(Import), "\n", imports(Imports), !.
imports([]) --> [].

prefixes([Prefix|Prefixes]) --> prefix(Prefix), "\n", prefixes(Prefixes), !.
prefixes([]) --> [].

axioms([Axiom|Axioms]) --> axiom(Axiom), "\n", axioms(Axioms), !.
axioms([]) --> [].

objectUnionOfExpression(objectUnionOf(Exp1, Exp2)) --> classExpression(Exp1), " ", objectUnionOfExpression(Exp2), !.
objectUnionOfExpression(Expression) --> classExpression(Expression).

objectIntersectionOfExpression(objectIntersectionOf(Exp1, Exp2)) --> classExpression(Exp1), " ", objectIntersectionOfExpression(Exp2), !.
objectIntersectionOfExpression(Expression) --> classExpression(Expression).

disjointExpression(disjoint(Exp1, Exp2)) --> class(Exp1), " ", disjointExpression(Exp2), !.
disjointExpression(Expression) --> class(Expression).

%%%%%%%%%%%%%%%%%
%% Helper Rules %
%%%%%%%%%%%%%%%%%

is_any_char(X) :- X >= 0, X < 255, X \== 32, X \== 41.

parse_owl(File, Prefixes, Imports, Axioms) :-
    read_file_to_codes(File, Input, []),
    owl(Prefixes, Imports, Axioms, Input, _).

%%%%%%%%%%%%%%%%%%%
%% Helper Clauses %
%%%%%%%%%%%%%%%%%%%

newline --> "\n", newline, !.
newline --> [].

any_chars([X|Y]) --> any_char(X), any_chars(Y).
any_chars([]) --> [].

any_char(X) --> [X], { is_any_char(X) }.

word(Word) --> chars(CHARS),
    { atom_codes(WORD, CHARS), downcase_atom(WORD, Word), !  }.

chars([X|Y]) --> char(X), chars(Y).
chars([]) --> [].

char(X) --> [X], { is_char(X) }.

is_char(X) :- X >= 0'a, X =< 0'z, !.
is_char(X) :- X >= 0'A, X =< 0'Z, !.
is_char(X) :- X >= 0'0, X =< 0'9, !.
is_char(0'_).

% TODO: Annotation, AnnotationAssertion, DataPropertyAssertion, 
% DataPropertyDomain, DataPropertyRange, DifferentIndividuals,
% DisjointClasses, EquivalentClasses, FunctionalObjectProperty,
% InverseObjectProperties, ObjectPropertyDomain, ObjectPropertyRange
% SubObjectPropertyOf, SymmetricObjectProperty, TransitiveObjectProperty
% ObjectHasValue, ObjectMaxCardinality, DisjointUnion,
% EquivalentObjectProperties, DisjointObjectProperties, SubDataPropertyOf,
% EquivalentDataProperties, DisjointDataProperties, FunctionalDataProperty,
% SameIndividual, NegativeObjectPropertyAssertion


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

dataPropertyAssertion(dataPropertyAssertion(Property)) --> 
    "DataPropertyAssertion(", entity(PropName), " ", entity(Ind1), " ", any_chars(Chars), ")",
        { name(Ind2, Chars), Property=..[PropName, Ind1, Ind2], ! }.

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

disjoint(Disjoint) -->
    "DisjointClasses(", disjointExpression(Disjoint), ")".

entity(Name) -->
    "<", any_chars(_), "#", word(Name), ">", { ! }.

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

propertyProperties(Pro) --> objectPropertyDomain(Pro), !.
propertyProperties(Pro) --> objectPropertyRange(Pro).

axiom(X) --> subClassOf(X), !.
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


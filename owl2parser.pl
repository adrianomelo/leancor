
owl(Prefixes, Imports, Axioms) --> 
    prefixes(Prefixes), "\n\n", ontology(Imports, Axioms).

prefixes([Prefix|Prefixes]) --> prefix(Prefix), "\n", prefixes(Prefixes), !.
prefixes([]) --> [].

prefix(prefix(Ns, Uri)) --> "Prefix(", word(Ns) ,":=", uri(Uri), ")".

ontology(Imports, Axioms) --> 
    "Ontology(", uri(_), "\n\n", imports(Imports), axioms(Axioms), ")", !.

%ontology(Imports, Axioms) --> 
%    "Ontology(<http://www.cin.ufpe.br/~astm/or.owl>\n\n", imports(Imports), axioms(Axioms), ")".

imports([Import|Imports]) --> import(Import), "\n", imports(Imports), !.
imports([]) --> [].

import(import(Uri)) --> "Import(", uri(Uri), ")".

declaration(class(Class)) -->
    "Declaration(Class(", entity(Name), "))",
        { Class=..[Name, _] }.

assertion(X) --> classAssertion(X).
assertion(X) --> objectPropertyAssertion(X).

classAssertion(classAssertion(Instance)) --> 
    "ClassAssertion(", entity(ClassName), " ", entity(InstanceName), ")", 
        { Instance=..[ClassName, InstanceName] }.

objectPropertyAssertion(propertyAssertion(Property)) --> 
    "ObjectPropertyAssertion(", entity(PropName), " ", entity(Ind1), " ", entity(Ind2), ")",
        { Property=..[PropName, Ind1, Ind2] }.

classExpression(Exp) --> class(Exp), !.
classExpression(Exp) --> objectSomeValuesFrom(Exp), !.
classExpression(Exp) --> objectAllValuesFrom(Exp), !.
classExpression(Exp) --> objectUnionOf(Exp), !.
classExpression(Exp) --> objectIntersectionOf(Exp).

objectSomeValuesFrom(objectSomeValuesFrom(Property, Expression)) -->
    "ObjectSomeValuesFrom(", property(Property), " ", classExpression(Expression), ")".

objectAllValuesFrom(objectAllValuesFrom(Property, Expression)) -->
    "ObjectAllValuesFrom(", property(Property), " ", classExpression(Expression), ")".

objectUnionOf(Union) --> 
    "ObjectUnionOf(", objectUnionOfExpression(Union), ")".
objectUnionOfExpression(objectUnionOf(Exp1, Exp2)) --> classExpression(Exp1), " ", objectUnionOfExpression(Exp2), !.
objectUnionOfExpression(Expression) --> classExpression(Expression).

objectIntersectionOf(Intersection) --> 
    "ObjectIntersectionOf(", objectIntersectionOfExpression(Intersection), ")".
objectIntersectionOfExpression(objectIntersectionOf(Exp1, Exp2)) --> classExpression(Exp1), " ", objectIntersectionOfExpression(Exp2), !.
objectIntersectionOfExpression(Expression) --> classExpression(Expression).

axioms([Axiom|Axioms]) --> axiom(Axiom), "\n", axioms(Axioms), !.
axioms([]) --> [].

axiom(X) --> declaration(X), !.
axiom(X) --> subClassOf(X), !.
axiom(X) --> disjoint(X).

subClassOf(subClassOf(Exp1, Exp2)) --> 
    "SubClassOf(", classExpression(Exp1), " ", classExpression(Exp2), ")".

disjoint(Disjoint) -->
    "DisjointClasses(", disjointExpression(Disjoint), ")".
disjointExpression(disjoint(Exp1, Exp2)) --> class(Exp1), " ", disjointExpression(Exp2), !.
disjointExpression(Expression) --> class(Expression).

property(Class) --> entity(PropertyName), { Class=..[PropertyName,_,_] }.
class(Class) --> entity(ClassName), { Class=..[ClassName,_] }.

entity(Name) --> "<", any_chars(_), "#", word(Name), ">", { ! }.

uri(Uri) --> "<", any_chars(Chars), "#>", { name(Uri, Chars), ! }.
uri(Uri) --> "<", any_chars(Chars), ">", { name(Uri, Chars) }.

%%%%%%%%%%%%%%%%%
%% Helper Rules %
%%%%%%%%%%%%%%%%%

is_any_char(X) :- X >= 0, X < 255, X \== 32, X \== 41.

%%%%%%%%%%%%%%%%%%%
%% Helper Clauses %
%%%%%%%%%%%%%%%%%%%

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

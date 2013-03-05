:- set_prolog_flag(toplevel_print_options, [quoted(true), portray(true)]).

atom_con2cat(Begin, Wanted, End, Input) :-
    atom_concat(Begin, RestA, Input),
    atom_concat(Wanted, End, RestA).

iri(IRIName) --> [In], {
    atom_concat(RestA, '>', In),
    atom_concat('<', RestD, RestA),
    atom_concat(RestC, RestB, RestD),
    atom_concat(_, '#', RestC),
    atom_codes(RestB, Codes),
    \+member(35, Codes),
    \+member(58, Codes),
    \+member(32, Codes),
    \+member(47, Codes),
    downcase_atom(RestB, IRIName)
}.

prefix(prefix(Prefix)) --> [In], {atom_con2cat('Prefix(', Prefix, ')', In)}.
import(import(URL))    --> [In], {atom_con2cat('Import(', URL, ')', In)}.

annotation(Annotation) --> annotationAssertion(Annotation).
axiom(Axiom) --> declaration(Axiom).
axiom(Axiom) --> subClassOf(Axiom).
axiom(Axiom) --> disjoint(Axiom).
axiom(Axiom) --> classAssertion(Axiom).

annotationAssertion(annotationAssertion(AnnotationAssertionValue)) -->
    [In], {atom_con2cat('AnnotationAssertion(', AnnotationAssertionValue, ')', In)}.

classAssertion(ClassAssertionValue) -->
    [In], {
        atom_con2cat('ClassAssertion(', Rest, ')', In),
        atomic_list_concat([Class, Instance], ' ', Rest),
        iri(ClassValue, [Class], []),
        iri(InstanceValue, [Instance], []),
        ClassAssertionValue=..[ClassValue, InstanceValue]
    }.

subClassOf(Expression) --> 
    [In], {
        atom_con2cat('SubClassOf(', Rest, ')', In),
        atomic_list_concat(List, ' ', Rest),
        complex_therm_subclassof(List, Expression), !
    }.

classExpression(Class) --> [In], {
    iri(ClassValue, [In], []),
    functor(Class, ClassValue, 1)
}.
    
classExpression(objectSomeValuesFrom(Property, ClassExpression)) -->
    [In], {
        atom_con2cat('ObjectSomeValuesFrom(', RestB, ')', In),
        atomic_list_concat([PropertyIRI|List], ' ', RestB),
        atomic_list_concat(List, ' ', ClassExpressionValue),
        classExpression(ClassExpression, [ClassExpressionValue], []),
        iri(FunctorName, [PropertyIRI], []),
        Property=..[FunctorName, X, Y], !
    }.

classExpression(objectAllValuesFrom(Property, Inside)) -->
    [In], {
        atom_con2cat('ObjectAllValuesFrom(', RestB, ')', In),
        atomic_list_concat([PropertyIRI|List], ' ', RestB),
        atomic_list_concat(List, ' ', ClassExpressionValue),
        classExpression(ClassExpression, [ClassExpressionValue], []),
        iri(FunctorName, [PropertyIRI], []),
        Property=..[FunctorName, X, Y],
        ClassExpression=..[ClassName, _],
        Inside=..[ClassName, Y]
    }.

classExpression(Expression) -->
    [In], {
        atom_con2cat('ObjectIntersectionOf(', RestB, ')', In),
        atomic_list_concat(List, ' ', RestB),
        complex_therm_intersection(List, Expression), !
    }.

classExpression(Expression) -->
    [In], {
        atom_con2cat('ObjectUnionOf(', RestB, ')', In),
        atomic_list_concat(List, ' ', RestB),
        complex_therm_union(List, Expression), !
    }.

classExpression(objectMinCardinality(Number, Property)) -->
    [In], {
        atom_con2cat('ObjectMinCardinality(', RestB, ')', In),
        atomic_list_concat([Number, Property], ' ', RestB)
    }.

classExpression(objectMaxCardinality(Number, Property)) -->
    [In], {
        atom_con2cat('ObjectMaxCardinality(', RestB, ')', In),
        atomic_list_concat([Number, Property], ' ', RestB)
    }.

classExpression(objectExactCardinality(Number, Property)) -->
    [In], {
        atom_con2cat('ObjectExactCardinality(', RestB, ')', In),
        atomic_list_concat([Number, Property], ' ', RestB)
    }.

classExpression(Class) -->
    [In], {
        iri(IRIName, [In], []),
        functor(Class, IRIName, 1)
    }.

classExpression(A) --> 
    [A], {
        \+atom_concat('ObjectExactCardinality', _, A),
        \+atom_concat('ObjectMaxCardinality', _, A),
        \+atom_concat('ObjectMinCardinality', _, A),
        \+atom_concat('ObjectSomeValuesFrom', _, A),
        \+atom_concat('ObjectAllValuesFrom', _,A),
        \+atom_concat('ObjectIntersectionOf', _, A),
        \+atom_concat('DisjointClasses',_,A),
        \+atomic_list_concat([_], ' ', A)
    }.

declaration(Entity) --> 
    [In], {
        atom_con2cat('Declaration(', DeclarationValue, ')', In),
        entity(Entity, [DeclarationValue], [])
    }.

    entity(Class) --> [In], {
        atom_con2cat('Class(', IRI, ')', In),
        iri(ClassValue, [IRI], []),
        functor(Class, ClassValue, 1)
    }.
    entity(dataType(DataTypeValue)) --> [In], {atom_con2cat('DataType(', DataTypeValue, ')', In)}.
    entity(dataProperty(DataPropertyValue)) --> [In], {atom_con2cat('DataProperty(', DataPropertyValue, ')', In)}.
    entity(Property) --> [In], {
		atom_con2cat('ObjectProperty(', ObjectPropertyValue, ')', In),
        iri(PropertyValue, [ObjectPropertyValue], []),
        functor(Property, PropertyValue, 2)
	}.

complex_therm_subclassof([HeadIn], Expression) :-
    classExpression(Expression, [HeadIn], []).

complex_therm_subclassof([HeadIn|In], Expression) :-
    classExpression(Out, [HeadIn], []),
    complex_therm_subclassof(In, Out2),
    Expression = subClassOf(Out, Out2).

complex_therm_subclassof([HeadIn|In], Expression):-
    append([HeadIn2|[]],Rest,In),
    atom_concat(HeadIn, ' ', Temp),
    atom_concat(Temp, HeadIn2, HeadIn3),
    append([HeadIn3], Rest, In2),
    complex_therm_subclassof(In2, Expression).

complex_therm_intersection([HeadIn], Expression) :-
    classExpression(Expression, [HeadIn], []).

complex_therm_intersection([HeadIn|In], Expression) :-
    classExpression(Out, [HeadIn], []),
    complex_therm_intersection(In, Out2),
    Expression = intersection(Out, Out2).

complex_therm_intersection([HeadIn|In], Expression):-
    append([HeadIn2|[]],Rest,In),
    atom_concat(HeadIn, ' ', Temp),
    atom_concat(Temp, HeadIn2, HeadIn3),
    append([HeadIn3], Rest, In2),
    complex_therm_intersection(In2, Expression).

complex_therm_union([HeadIn], Expression) :-
    classExpression(Expression, [HeadIn], []).

complex_therm_union([HeadIn|In], Expression) :-
    classExpression(Out, [HeadIn], []),
    complex_therm_union(In, Out2),
    Expression = union(Out, Out2).

complex_therm_union([HeadIn|In], Expression):-
    append([HeadIn2|[]],Rest,In),
    atom_concat(HeadIn, ' ', Temp),
    atom_concat(Temp, HeadIn2, HeadIn3),
    append([HeadIn3], Rest, In2),
    complex_therm_union(In2, Expression).

disjoint(Expression) -->
    [In], {
        atom_con2cat('DisjointClasses(',RestB,')',In),
        atomic_list_concat(List, ' ', RestB),
        disjoint_classes(List, Expression), !
    }.

disjoint_classes([Element], IRI) :-
    classExpression(IRI, [Element], []), !.

disjoint_classes([HeadIn|In], Expression) :-
    classExpression(IRI, [HeadIn], []),
    disjoint_classes(In, Out2),
    Expression = disjoint(IRI, Out2), !.
    
parse_owl(File, Prefixes, Imports, Annotations, Axioms) :-
    read_file_to_codes(File, Codes, []),
    atom_codes(OntologyDocument, Codes),
    parse_prefixes(OntologyDocument, Prefixes, Ontology),
    parse_ontology(Ontology, Imports, Annotations, Axioms).

parse_prefixes(OntologyDocument, Prefixes, Ontology) :-
    atomic_list_concat(List, '\n', OntologyDocument),
    parse_prefix(List, Prefixes, Ontology).

parse_prefix([], [], _) :- !.
parse_prefix([Head|Rest], Tree, Ontology) :- Head == '', parse_prefix(Rest, Tree, Ontology).
parse_prefix([Head|Rest], [Element|Tree], Ontology) :- prefix(Element, [Head], []), parse_prefix(Rest, Tree, Ontology).
parse_prefix(List, _, Ontology) :- atomic_list_concat(List, '\n', Ontology), atom_concat('Ontology', _, Ontology), !.

parse_ontology(Ontology, Imports, Annotations, Axioms) :- 
    atom_con2cat('Ontology(', RestB, ')\n', Ontology),
    atomic_list_concat(List, '\n', RestB),
    %parse_imports(List, Imports),
    %parse_annotations(List, Annotations),
    parse_axioms(List, Axioms).

parse_imports([], []) :- !.
parse_imports([Head|Rest], [Import|Imports]) :- import(Import, [Head], []), parse_imports(Rest, Imports).
parse_imports([Head|Rest], Imports) :- \+import(_, [Head], []), parse_imports(Rest, Imports).

parse_annotations([], _) :- !.
parse_annotations([Head|Rest], [Annotation|Annotations]) :- annotation(Annotation, [Head], []), parse_annotations(Rest, Annotations).
parse_annotations([_|Rest], Annotations) :- \+parse_annotations(Rest, Annotations).

parse_axioms([], []) :- !.
parse_axioms([Head|Rest], [Axiom|Axioms]) :- axiom(Axiom, [Head], []), parse_axioms(Rest, Axioms).
parse_axioms([Head|Rest], Axioms) :- \+axiom(_, [Head], []), parse_axioms(Rest, Axioms).

%print([]) :- writeln('\n').
%print([A|B]) :- writeln(A), print(B).

create_matrix([], []).
create_matrix([Head|AxiomList], Matrix) :-
    to_clausule(Head, []),
    create_matrix(AxiomList, Matrix), !.
    
create_matrix([Head|AxiomList], Ret) :-
    to_clausule(Head, Clausule),
    append(Clausule, Matrix, Ret),
    create_matrix(AxiomList, Matrix).

% This code can be improved. recursion needed!
%to_clausule(subClassOf(objectSomeValuesFrom(A, B), C), M) :- M = [[A, B, -C]], !.
%to_clausule(subClassOf(A, objectSomeValuesFrom(B, C), M)) :- M = [[A, -B], [A, -C]], !.
%to_clausule(subClassOf(union(A, B), C), M) :- M = [[A, -C], [B, -C]], !.
%to_clausule(subClassOf(A, union(B, C)), M) :- M = [[A, -B, -C]], !.
%to_clausule(subClassOf(intersection(objectSomeValuesFrom(A, B), C), M)) :- M = [[A, B, -C]], !.
%to_clausule(subClassOf(intersection(A, B), C), M) :- M = [[A, B, -C]], !.
%to_clausule(subClassOf(A, intersection(B, C)), M) :- M = [[A, -B], [A, -C]], !.
%to_clausule(subClassOf(A, -B), M) :- M = [[A, B]], !.
%to_clausule(subClassOf(A, B), M) :- M = [[A, -B]], !.
%to_clausule(Item, []) :-
%    \+Item = subClassOf(_, _).

to_clausule(subClassOf(A, B), Matrix) :-
	to_clausule_left(A, Ad),
	to_clausule_right(B, Bd),
	append(Ad, Bd, M),
	remove_rows(M, Matrix),
	!.
	
to_clausule(Item, []) :- \+Item=subClassOf(_,_), !.

to_clausule_left(A, [A]) :- var(A), !.
to_clausule_left(union(A, B), [M]) :- to_clausule_left(A, Ad), to_clausule_left(B, Bd), append(Ad, Bd, M), !.
to_clausule_left(intersection(A, B), M) :- to_clausule_left(A, Ad), to_clausule_left(B, Bd), append(Ad, Bd, M), !.
to_clausule_left(objectSomeValuesFrom(A, B), M) :- to_clausule_left(A, Ad), to_clausule_left(B, Bd), append(Ad, Bd, M), !.
to_clausule_left(A, [A]) :- !.

to_clausule_right(A, [-A]) :- var(A), !.
to_clausule_right(union(A, B), M) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
to_clausule_right(intersection(A, B), [M]) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
to_clausule_right(objectAllValuesFrom(A, B), M) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
to_clausule_right(objectSomeValuesFrom(A, B), [M]) :- to_clausule_right(A, Ad), to_clausule_right(B, Bd), append(Ad, Bd, M), !.
to_clausule_right(-A, [A]) :- !.
to_clausule_right(A, [-A]).

remove_rows([], [[]]).
remove_rows([Head|In], Processed2) :-
	is_list(Head),
	remove_rows(In, Processed1),
	append_list(Head, Processed1, Processed2), !.
	
remove_rows([Head|In], Processed2) :-
	remove_rows(In, Processed1),
	append_element(Head, Processed1, Processed2).

append_list([], _, []).	
append_list([ListHead|List], MatrixInput, Matrix) :-
	append_list(List, MatrixInput, MatrixOutput1),
	append_element(ListHead, MatrixInput, MatrixOutput2),
	append(MatrixOutput1, MatrixOutput2, Matrix).
	
append_element(_, [], []).
append_element(Element, [Head|Matrix], [Head2|Output]) :-
	append([Element], Head, Head2),
	append_element(Element, Matrix, Output).

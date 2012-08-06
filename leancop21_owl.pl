:- set_prolog_flag(toplevel_print_options, [quoted(true), portray(true)]).

atom_con2cat(Begin, Wanted, End, Input) :-
    atom_concat(Begin, RestA, Input),
    atom_concat(Wanted, End, RestA).

prefix(prefix(Prefix)) --> [In], {atom_con2cat('Prefix(', Prefix, ')', In)}.
import(import(URL))    --> [In], {atom_con2cat('Import(', URL, ')', In)}.

annotation(Annotation) --> annotationAssertion(Annotation).
axiom(Axiom) --> declaration(Axiom).
axiom(Axiom) --> subClassOf(Axiom).

annotationAssertion(annotationAssertion(AnnotationAssertionValue)) -->
    [In], {atom_con2cat('AnnotationAssertion(', AnnotationAssertionValue, ')', In)}.

subClassOf(subClassOf(class(Parent), ClassExpression)) --> 
    [In], {
        atom_con2cat('SubClassOf(', SubClassOfValue, ')', In),
        atomic_list_concat([Parent|List], ' ', SubClassOfValue),
        atomic_list_concat(List, ' ', ClassExpressionValue),
        classExpression(ClassExpression, [ClassExpressionValue], [])
    }.

    classExpression(class(Class)) --> [In], {atomic_list_concat([Class], ' ', In)}.
    
    classExpression(objectSomeValuesFrom(objectProperty(Property), ClassExpression)) -->
        [In], {
            atom_con2cat('ObjectSomeValuesFrom(', RestB, ')', In),
            atomic_list_concat([Property|List], ' ', RestB),
            atomic_list_concat(List, ' ', ClassExpressionValue),
            classExpression(ClassExpression, [ClassExpressionValue], [])
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

    classExpression(A) --> 
        [A], {
            \+atom_concat('ObjectExactCardinality', _, A),
            \+atom_concat('ObjectMaxCardinality', _, A),
            \+atom_concat('ObjectMixtCardinality', _, A),
            \+atom_concat('ObjectSomeValuesFrom', _, A),
            \+atomic_list_concat([B], ' ', A)
        }.

declaration(Entity) --> 
    [In], {
        atom_con2cat('Declaration(', DeclarationValue, ')', In),
        entity(Entity, [DeclarationValue], [])
    }.

    entity(class(ClassValue)) --> [In], {atom_con2cat('Class(', ClassValue, ')', In)}.
    entity(dataType(DataTypeValue)) --> [In], {atom_con2cat('DataType(', DataTypeValue, ')', In)}.
    entity(dataProperty(DataPropertyValue)) --> [In], {atom_con2cat('DataProperty(', DataPropertyValue, ')', In)}.
    entity(objectProperty(ObjectPropertyValue)) --> [In], {atom_con2cat('ObjectProperty(', ObjectPropertyValue, ')', In)}.
	
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
    parse_imports(List, Imports),
    parse_annotations(List, Annotations),
    parse_axioms(List, Axioms).

parse_imports([], []) :- !.
parse_imports([Head|Rest], [Import|Imports]) :- import(Import, [Head], []), parse_imports(Rest, Imports).
parse_imports([Head|Rest], Imports) :- \+import(_, [Head], []), parse_imports(Rest, Imports).

parse_annotations([], _) :- !.
parse_annotations([Head|Rest], [Annotation|Annotations]) :- annotation(Annotation, [Head], []), parse_annotations(Rest, Annotations).
parse_annotations([Head|Rest], Annotations) :- \+parse_annotations(Rest, Annotations).

parse_axioms([], []) :- !.
parse_axioms([Head|Rest], [Axiom|Axioms]) :- axiom(Axiom, [Head], []), parse_axioms(Rest, Axioms).
parse_axioms([Head|Rest], Axioms) :- \+axiom(Axiom, [Head], []), parse_axioms(Rest, Axioms).

print([]) :- writeln('\n').
print([A|B]) :- writeln(A), print(B).


create_matrix([], _).
create_matrix([Head|AxiomList], Matrix) :-
    to_clausule(Head, Clausule),
    (Clausule == [] -> Matrix = Ret; Matrix = [Clausule|Ret]),
    create_matrix(AxiomList, Ret).

to_clausule(subClassOf(A, B), M) :- M = [[A], [-B]].
to_clausule(Item, []) :-
    \+Item = subClassOf(_, _).



:- [owl2_fol].
:- [owl2_parser].
:- [owl2_output].
:- [owl2_operators].
:- [leancop21_swi].
:- [leancop_tptp2].

:- dynamic(subclassof/2).
:- dynamic(prefix/2).
:- dynamic(class/2).
:- dynamic(consistent/1).

%%%%%%%%%%%%%%%%%%
% Activities API %
%%%%%%%%%%%%%%%%%%

consistency(OperationTime) :-
    setup_matrix(consistency),
    get_time(Start),
    asserta(consistent(true)),
    (prove(1, [cut,comp(7),scut], _) ->
        asserta(consistent(false)); true),
    get_time(End),
    OperationTime is round((End - Start) * 1000),
    write_consistency_output.

test_consistency(Class) :-
  A=..[Class, _],
  asserta(lit(-A, -A, [], n)),
  (prove(1, [cut,scut,comp(7)], _) ->
      asserta(consistent(false)); true),
  retract(lit(-A, -A, [], n)).

classify(OperationTime) :-
    setup_matrix(classification),
    get_time(Start),
    forall((class(_,A),class(_,B),A\=B,not(subclassof(A,B))), test_subsumption(A,B)),
    get_time(End),
    write_classification_output_file,
    OperationTime is round((End - Start) * 1000),
    write_debug_tuple('Classification time', OperationTime), !.

test_subsumption(Specific, Concept) :-
    A=..[Specific, c],
    B=..[Concept, c],
    asserta(lit(-A, -A, [], g)),
    (prove(B, 1, [cut,comp(7)], _) ->
        asserta(subclassof(Specific, Concept)); true),
    retract(lit(-A, -A, [], g)).

prove(Literal,PathLim,Set,Proof) :-
    prove([Literal],[],PathLim,[],Set,Proof).

prove(Literal,PathLim,Set,Proof) :-
    member(comp(Limit),Set),
    PathLim\=Limit,
    PathLim1 is PathLim+1, prove(Literal,PathLim1,Set,Proof).

%%%%%%%%%
% Setup %
%%%%%%%%%

setup_matrix(Activity) :-
    load_ontology(Prefixes, Axioms),
    process_ontology(Prefixes, Axioms, NewAxioms),
    append(Axioms, NewAxioms, ProcessedAxioms),
    axiom_list_to_matrix(Activity, ProcessedAxioms, Fol, Matrix),
    assert_clauses(Matrix, pos), !.
    %write_debug(Axioms, Fol, Matrix), !.

process_ontology(Prefixes, Axioms, NewAxioms) :-
    process_prefixes(Prefixes),
    process_axioms(Axioms),
    post_process(NewAxioms),
    process_axioms(NewAxioms).

%%%%%%%%%%%%%%%%%%%%
% Axioms to matrix %
%%%%%%%%%%%%%%%%%%%%

axiom_list_to_matrix(consistency, Axioms, Fol, Matrix) :-
  findall((not X), (class(_, X), X\=='owl:Thing'), RightHandAxioms),
  list_to_or_operator2(RightHandAxioms, RightHandAxiomsOr),
  to_fol('owl:Thing' is_a RightHandAxiomsOr, RightHandFol),
  axiom_list_to_and_formula(Axioms, Fol),
  basic_equal_axioms(F),
  make_matrix((~Fol; ~F; RightHandFol), Matrix, []), !.

axiom_list_to_matrix(classification, Axioms, Fol, Matrix) :-
  axiom_list_to_and_formula(Axioms, Fol),
  basic_equal_axioms(F),
  make_matrix(~(Fol, F), Matrix, []), !.

axiom_list_to_matrix(realisation, _, _, _).

axiom_list_to_and_formula(Axioms, Fol) :-
    axioms_to_fol(Axioms, Formulas),
    list_to_and_operator(Formulas, Fol).

axioms_to_fol([], []).
axioms_to_fol([Head|Axioms], Fol) :-
    to_fol(Head, NewFol),
    NewFol \= [],
    append([NewFol], Formulas, Fol),
    axioms_to_fol(Axioms, Formulas).
axioms_to_fol([_|Axioms], Fol) :-
    axioms_to_fol(Axioms, Fol).

%%%%%%%%%%%%%%%%%%%%%%%
% Loading and Parsing %
%%%%%%%%%%%%%%%%%%%%%%%

load_ontology(Prefixes, Axioms) :-
    get_time(Start1),
    file(input, OntologyFile),
    parse_owl(OntologyFile, Prefixes, _, Axioms),
    get_time(End1),
    OperationTime1 is round((End1 - Start1) * 1000),
    write_debug_tuple('Parsing time', OperationTime1).

parse_owl(File, Prefixes, Imports, Axioms) :-
    read_file_to_codes(File, Input, []),
    (phrase(owl(Prefixes, Imports, Axioms), Input, _) ->
        write_debug_tuple('Parsed', 'true')
        ; % print debug information
        write_debug_tuple('Parsed', 'false'),
        atom_codes(InputAtom, Input),
        atomic_list_concat([_TODO_PreOnto, PostOnto], 'Ontology', InputAtom),
        atomic_list_concat(AxiomList, '\n', PostOnto),
        forall((member(Ax, AxiomList), atom_codes(Ax, AxInput)), (axiom(_, AxInput, []) -> true; write_debug_tuple(Ax, 'failed'))),
        fail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database and matrix management %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_prefixes([]).
process_prefixes([Head|List]) :-
    assert(Head),
    process_prefixes(List).

process_axioms([]).
process_axioms([class [Class, Uri]|Axioms]) :-
    assert(class(Class, Uri)),
    process_axioms(Axioms), !.

process_axioms([A is_a B|Axioms]) :-
    atom(A), atom(B),
    A\='owl:Nothing',B\='owl:Nothing',
    assert(subclassof(A, B)),
    process_axioms(Axioms).

process_axioms([A equivalent B|Axioms]) :-
    atom(A), atom(B),
    assert(subclassof(A, B)),
    assert(subclassof(B, A)),
    process_axioms(Axioms).

process_axioms([_|Axioms]) :-
    process_axioms(Axioms).

post_process(M) :-
    findall(X is_a 'owl:Thing', class(_, X), M1),
    M2=[class [thing, 'owl:Thing']],
    append(M1, M2, M).

list_to_and_operator([A, B], (A, B)).
list_to_and_operator([A|B], (A, D)) :-
    list_to_and_operator(B, D).

list_to_or_operator([A, B], (A; B)) :- !.
list_to_or_operator([A|B], (A; D)) :-
    list_to_or_operator(B, D).

list_to_or_operator2([A, B], (A or B)) :- !.
list_to_or_operator2([A|B], (A or D)) :-
    list_to_or_operator2(B, D).

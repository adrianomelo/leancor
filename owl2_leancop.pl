:- [owl2_fol].
:- [owl2_parser].
:- [owl2_output].
:- [leancop21_swi].
:- [leancop_tptp2].

:- dynamic(subclassof/2).
:- dynamic(prefix/2).

%%%%%%%%%%%%%%%%%%
% Activities API %
%%%%%%%%%%%%%%%%%%

classify(InputOntologyFile, OperationTime, OutputOntologyFile) :-
    setup_matrix(InputOntologyFile, Concepts),
    get_time(Start),
	test_subsumption_list(Concepts, Concepts),
	get_time(End),
    write_classification_output_file(OutputOntologyFile),
	OperationTime is round((End - Start) * 1000).

test_subsumption_list(_, []).
test_subsumption_list(AllConcepts, [Concept|Concepts]) :-
	test_subsumption(AllConcepts, Concept),
	test_subsumption_list(AllConcepts, Concepts).

test_subsumption([], _).
test_subsumption([Specific|Concepts], Concept) :-
    not(subclassof(Specific, Concept)),
    Specific \= Concept,
    A=..[Specific, c],
    B=..[Concept, c],
    asserta(lit(-A, -A, [], g)),
    (prove(B, 1, [cut,comp(7)], _) ->
        asserta(subclassof(Specific, Concept)); true),
    retract(lit(-A, -A, [], g)),
	test_subsumption(Concepts, Concept), !.

test_subsumption([_|Concepts], Concept) :-
    test_subsumption(Concepts, Concept).

prove(Literal,PathLim,Set,Proof) :-
    prove([Literal],[],PathLim,[],Set,Proof).

prove(Literal,PathLim,Set,Proof) :-
    member(comp(Limit),Set),
    PathLim\=Limit,
    PathLim1 is PathLim+1, prove(Literal,PathLim1,Set,Proof).

%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%

setup_matrix(OntologyFile, Concepts) :-
	owl2_to_matrix(OntologyFile, Matrix, Concepts),
    assert_clauses(Matrix, conj).

owl2_to_matrix(File, Matrix, Concepts) :-
	parse_owl(File, Prefixes, _, Axioms),
	axioms_to_fol(Axioms, Formulas),
    list_to_operator(Formulas, Fol),
    make_matrix(~(Fol), KBMatrix, []),
    basic_equal_axioms(F),
    make_matrix(~(F), EqMatrix, []),
    append(KBMatrix, EqMatrix, Matrix),
    write_debug(File, Axioms, Fol, Matrix),
	process_prefixes(Prefixes),
	process_axioms(Axioms, Concepts).

process_prefixes([]).
process_prefixes([Head|List]) :-
    assert(Head),
    process_prefixes(List).

process_axioms([], []).
process_axioms([class(Concept)|Axioms], [Concept|Concepts]) :-
	process_axioms(Axioms, Concepts), !.
process_axioms([A is_a B|Axioms], Concepts) :-
    atom(A), atom(B),
    assert(subclassof(A, B)),
    process_axioms(Axioms, Concepts).
 process_axioms([A same_as B|Axioms], Concepts) :-
    atom(A), atom(B),
    assert(subclassof(A, B)),
    assert(subclassof(B, A)),
    process_axioms(Axioms, Concepts).
process_axioms([_|Axioms], Concepts) :-
	process_axioms(Axioms, Concepts).


axioms_to_fol([], []).
axioms_to_fol([Head|Axioms], Fol) :-
    to_fol(Head, NewFol),
    NewFol \= [],
    append([NewFol], Formulas, Fol),
    axioms_to_fol(Axioms, Formulas).
axioms_to_fol([_|Axioms], Fol) :-
    axioms_to_fol(Axioms, Fol).

list_to_operator([A, B], (A, B)).
list_to_operator([A|B], (A, D)) :-
    list_to_operator(B, D).

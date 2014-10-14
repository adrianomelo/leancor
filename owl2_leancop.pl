:- [owl2_fol].
:- [owl2_matrix].
:- [owl2_parser_operators].
:- [owl2_output].
:- [leancop21_swi].

:- dynamic(subclassof/3).
:- dynamic(prefix/2).

%%%%%%%%%%%%%%%%%%
% Activities API %
%%%%%%%%%%%%%%%%%%

classify(InputOntologyFile, OperationTime, OutputOntologyFile) :-
	owl2_to_matrix(InputOntologyFile, Matrix, Concepts),
	get_time(Start),
	test_subsumption_list(Matrix, Concepts, Concepts),
	get_time(End),
    write_classification_output_file(OutputOntologyFile),
	OperationTime is round((End - Start) * 1000).

test_subsumption_list(_, _, []).
test_subsumption_list(Matrix, AllConcepts, [Concept|Concepts]) :-
	test_subsumption(Matrix, AllConcepts, Concept),
	test_subsumption_list(Matrix, AllConcepts, Concepts).

test_subsumption(_, [], _).
test_subsumption(Matrix, [Specific|Concepts], Concept) :-
    not(subclassof(Specific, Concept, _)),
    Specific \= Concept,
    A=..[Specific, c],
    B=..[Concept, c],
    %print(Matrix),
	append([[-(#)], [-A], [B, #]], Matrix, MatrixWithQuery),
	%print('Test'), print(A), print(B),print('\n'),
    %print(MatrixWithQuery),print('\n'),
	prove(MatrixWithQuery,_),
    assert(subclassof(Specific, Concept, i)),
	test_subsumption(Matrix, Concepts, Concept), !.

test_subsumption(Matrix, [_|Concepts], Concept) :-
	test_subsumption(Matrix, Concepts, Concept).

%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%

owl2_to_matrix(File, Matrix, Concepts) :-
	parse_owl(File, Prefixes, _, Axioms),
	axioms_to_fol(Axioms, Formulas),
	%print(Formulas),print('\n'),
    list_to_operator(Formulas, Fol),
	%print(Fol),print('\n'),
    make_matrix(~(Fol), Matrix, []),
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
    atom(A),
    atom(B),
    assert(subclassof(A, B, o)),
    process_axioms(Axioms, Concepts).
process_axioms([_|Axioms], Concepts) :-
	process_axioms(Axioms, Concepts). 

owl2_info(File) :-
	parse_owl(File, _, _, Axioms),
	print_info(Axioms).

print_info([]).
print_info([Head|Axioms]) :-
	print('Axiom: '), print(Head), print('\n'),
    to_fol(Head, Formula),
	print('Fol: '), print(Formula), print('\n'),
	make_matrix(~(Formula), Matrix, []),
	print('Neg Matrix: '), print(Matrix), print('\n'),
	print_info(Axioms).

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

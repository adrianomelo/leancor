:- [owl2_fol].
:- [owl2_matrix].
:- [owl2_parser_operators].
:- [leancop21_swi].

:- dynamic(intance/1).

%%%%%%%%%%%%%%%%%%
% Activities API %
%%%%%%%%%%%%%%%%%%

classify(InputOntologyFile, OperationTime, _OutputOntologyFile) :-
	owl2_to_matrix(InputOntologyFile, Matrix, Concepts),
	get_time(Start),
	test_subsumption_list(Matrix, Concepts, Concepts, _TODO2),
	get_time(End),
	OperationTime is round((End - Start) * 1000).

%%%%%%%%%%%%%%%
% Subsumption %
%%%%%%%%%%%%%%%

test_subsumption_list(_, _, [], []).
test_subsumption_list(Matrix, AllConcepts, [Concept|Concepts], AllPairs) :-
	test_subsumption(Matrix, AllConcepts, Concept, SubClasses),
	print('SubClassesOf '),print(Concept),print(' '),print(SubClasses),print('\n'),
	test_subsumption_list(Matrix, AllConcepts, Concepts, PairsOthers),
	append([[Concept, SubClasses]], PairsOthers, AllPairs).

test_subsumption(_, [], _, []).
test_subsumption(Matrix, [Subsumer|Concepts], Concept, [Subsumer|SubClasses]) :-
    Subsumer \= Concept,
    A=..[Subsumer, c],
    B=..[Concept, c],
	append([[-(#)], [A], [-B, #]], Matrix, MatrixWithQuery),
    print(MatrixWithQuery), print('\n'),
	prove(MatrixWithQuery,_),
	test_subsumption(Matrix, Concepts, Concept, SubClasses), !.

test_subsumption(Matrix, [_|Concepts], Concept, Pairs) :-
	test_subsumption(Matrix, Concepts, Concept, Pairs).

%%%%%%%%%%%
% LeanCop %
%%%%%%%%%%%

initialize(Matrix) :-
	retractall(lit(_,_,_,_)),
	(member([-(#)],Matrix) -> S=conj ; S=pos),
	assert_clauses(Matrix, S).

%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%

owl2_to_matrix(File, Matrix, Concepts) :-
	parse_owl(File, _, _, Axioms),
	axioms_to_fol(Axioms, Matrix),
	print(Matrix),print('\n'),
	axioms_subclassof(Axioms, Concepts).

axioms_subclassof([], []).
axioms_subclassof([class(Concept)|Axioms], [Concept|Concepts]) :-
	axioms_subclassof(Axioms, Concepts), !.
axioms_subclassof([_|Axioms], Concepts) :-
	axioms_subclassof(Axioms, Concepts). 

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
    make_matrix(~(NewFol), Matrix, []),
    axioms_to_fol(Axioms, Formulas),
    append(Matrix, Formulas, Fol).
axioms_to_fol([_|Axioms], Fol) :-
    axioms_to_fol(Axioms, Fol).

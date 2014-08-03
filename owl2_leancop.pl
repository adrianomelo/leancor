:- [owl2_parser].
:- [owl2_matrix].
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
	%print('SubClassesOf '),print(Concept),print(' '),print(SubClasses),print('\n'),
	test_subsumption_list(Matrix, AllConcepts, Concepts, PairsOthers),
	append([[Concept, SubClasses]], PairsOthers, AllPairs).

test_subsumption(_, [], _, []).
test_subsumption(Matrix, [TestConcept|Concepts], Concept, [TestConcept|SubClasses]) :-
	Ghost=instance(_),
	instanciate(TestConcept, NewTestConcept, Ghost),
	instanciate(Concept, NewConcept, Ghost),
	append([[-(#)], [-NewTestConcept], [NewConcept, #]], Matrix, MatrixWithQuery),
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

owl2_to_matrix(File, Matrix) :-
	parse_owl(File, _, _, Axioms),
	create_matrix(Axioms, Matrix).

owl2_to_matrix(File, Matrix, Concepts) :-
	parse_owl(File, _, _, Axioms),
	create_matrix(Axioms, Matrix),
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
	create_matrix([Head], Matrix),
	print('Matrix: '), print(Matrix), print('\n'),
	print_info(Axioms).

instanciate(Concept, NewConcept, Instance) :-
	Concept=..[ConceptName,_],
	NewConcept=..[ConceptName,Instance].

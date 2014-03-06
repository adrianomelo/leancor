:- [owl2_parser].
:- [owl2_matrix].
:- [leancop21_swi].

%%%%%%%%%%%%%%%%%%
% Activities API %
%%%%%%%%%%%%%%%%%%

classify(FileIn, FileOut) :-
	owl2_to_matrix(FileIn, Matrix, Concepts),
	test_subsumption_list(Matrix, Concepts, Concepts, Pairs).

%%%%%%%%%%%%%%%
% Subsumption %
%%%%%%%%%%%%%%%

test_subsumption_list(_, _, [], []).
test_subsumption_list(Matrix, AllConcepts, [Concept|Concepts], AllPairs) :-
	test_subsumption(Matrix, AllConcepts, Concept, Pairs),
	test_subsumption_list(Matrix, AllConcepts, Concepts, PairsOthers),
	append(Pairs, PairsOthers, AllPairs).

test_subsumption(_, [], _, []).
test_subsumption(Matrix, [TestConcept|Concepts], Concept, Pairs) :-
	print(TestConcept), print(' subclassof '), print(Concept), print('\n'),
	test_subsumption(Matrix, Concepts, Concept, Pairs).

%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%

owl2_to_matrix(File, Matrix) :-
	parse_owl(File, Prefixes, Imports, Axioms),
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
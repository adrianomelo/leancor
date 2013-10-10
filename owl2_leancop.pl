:- [owl2_parser].
:- [owl2_matrix].
:- [leancop21_swi].

owl2_to_matrix(File, Matrix) :-
	parse_owl(File, Prefixes, Imports, Axioms),
	create_matrix(Axioms, Matrix).

owl2_info(File) :-
	parse_owl(File, _, _, Axioms),
	print_info(Axioms).

print_info([]).
print_info([Head|Axioms]) :-
	print_info(Axioms),
	print('Axiom: '), print(Head), print('\n'),
	create_matrix([Head], Matrix),
	print('Matrix: '), print(Matrix), print('\n').
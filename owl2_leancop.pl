:- [owl2_parser].
:- [owl2_matrix].

owl_to_matrix(File, Matrix) :-
	parse_owl(File, Prefixes, Imports, Axioms),
	%print('Axioms: '), print(Axioms), print('\n'),
	create_matrix(Axioms, Matrix).
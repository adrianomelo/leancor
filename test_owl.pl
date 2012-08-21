:- [leancop21_owl].

:- begin_tests(owlparser).

test(classDeclaration) :-
	Input  = 'Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))',
	Output = class(animal(_)),
	declaration(Output, [Input], []).

test(prefix) :-
    Input  = 'Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)',
    Output = prefix('rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>'),
    prefix(Output, [Input], []).

test(ontologyImport) :-
    Input  = 'Import(<http://www.co-ode.org/ontologies/pizza/pizza.owl>)',
    Output = import('<http://www.co-ode.org/ontologies/pizza/pizza.owl>'),
    import(Output, [Input], []).

:- end_tests(owlparser).
:- begin_tests(creatematrix).

test(class1) :-
	Input  = [class(classname(_))],
	Output = [],
	create_matrix(Input, Output).

test(subsumption1) :-
	Input  = [subClassOf(class(a(_)), class(b(_)))],
	Output = [[class(a(_)), -class(b(_))]],
	create_matrix(Input, Output).

test(subsumption2) :-
	Input  = [subClassOf(class(a(_)), class(b(_))), subClassOf(class(c(_)), class(d(_)))],
	Output = [[class(a(_)), -class(b(_))], [class(c(_)), -class(d(_))]],
	create_matrix(Input, Output).

test(disjunction1) :-
	Input  = [subClassOf(class(a(_)), -class(b(_)))],
	Output = [[class(a(_)), class(b(_))]],
	create_matrix(Input, Output).

test(disjunction2) :-
	Input  = [subClassOf(class(a(_)), -class(b(_))), subClassOf(class(c(_)), -class(d(_)))],
	Output = [[class(a(_)), class(b(_))], [class(c(_)), class(d(_))]],
	create_matrix(Input, Output).

test(lefthandsideunion1) :-
	Input  = [subClassOf(union(class(a(_)), class(b(_))), class(c(_)))],
	Output = [[class(a(_)), -class(c(_))], [class(b(_)), -class(c(_))]],
	create_matrix(Input, Output).

test(righthandsideunion1) :-
	Input  = [subClassOf(class(a(_)), union(class(b(_)), class(c(_))))],
	Output = [[class(a(_)), -class(b(_)), -class(c(_))]],
	create_matrix(Input, Output).

test(lefthandsideintersection1) :-
	Input  = [subClassOf(intersection(class(a(_)), class(b(_))), class(c(_)))],
	Output = [[class(a(_)), class(b(_)), -class(c(_))]],
	create_matrix(Input, Output).

test(righthandsideintersection1) :-
	Input  = [subClassOf(class(a(_)), intersection(class(b(_)), class(c(_))))],
	Output = [[class(a(_)), -class(b(_))], [class(a(_)), -class(c(_))]],
	create_matrix(Input, Output).

%test(lefthandsideexistencialrestriction1) :-
%	Input  = [subClassOf(objectSomeValuesFrom(objectProperty('p'), class('A')), class('B'))],
%	Output = [[class('A'), class('B'), -class('C')]],
%	create_matrix(Input, Output).

%test(righthandsideexistencialrestriction1) :-
%	Input  = [subClassOf(class('A'), objectSomeValuesFrom(objectProperty('p'), class('B')))],
%	Output = [[class('A'), class('B'), -class('C')]],
%	create_matrix(Input, Output).

%test(lefthandsideuniversalrestriction1) :-
%	Input  = [subClassOf(ObjectSomeValuesFrom(objectProperty('p'), class('A')), class('B'))],
%	Output = [[class('A'), class('B'), -class('C')]],
%	create_matrix(Input, Output).

%test(righthandsideuniversalrestriction1) :-
%	Input  = [subClassOf(class('A'), ObjectSomeValuesFrom(objectProperty('p'), class('B')))],
%	Output = [[class('A'), class('B'), -class('C')]],
%	create_matrix(Input, Output).

:- end_tests(creatematrix).
:- begin_tests(leancop).

:- [leancop_main].

test(owl1) :-
	parse_owl('testfiles/bird.owl', _, _, _, Axioms),
	create_matrix(Axioms, Matrix),
	prove(Matrix, Proof),
	.\Proof == [].

:- end_tests(leancop).
:- run_tests.

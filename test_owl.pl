:- [leancop21_owl].

:- begin_tests(owlparser).

test(classDeclaration) :-
    declaration(class('<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>'),
                ['Declaration(Class(<http://www.cin.ufpe.br/~astm/owl/bird.owl#Animal>))'], []).

test(prefix) :-
    Input  = 'Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)',
    Output = prefix('rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>'),
    prefix(Output, [Input], []).

test(ontologyImport) :-
    Input  = 'Import(<http://www.co-ode.org/ontologies/pizza/pizza.owl>)',
    Output = import('<http://www.co-ode.org/ontologies/pizza/pizza.owl>'),
    import(Output, [Input], []).

:- end_tests(owlparser).
:- run_tests.


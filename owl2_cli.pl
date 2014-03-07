:- [owl2_leancop].
:- [leancop21_swi].

ore :-
    current_prolog_flag(argv, Argv),
    activity(Argv).

activity([consistency, Ontology, CSVOutput]) :-
    list_p(['Consistency check of', Ontology, 'with output', CSVOutput]).

activity([classification, Ontology, OntologyOut]) :-
    list_p(['Classification of', Ontology, 'with output', OntologyOut]),
    classify(Ontology, OntologyOut).

activity([sat, Ontology, CSVOutput, Concept]) :-
    list_p(['Sat of', Concept, 'in', Ontology, 'with output', CSVOutput]).

list_p(List) :-
    atomic_list_concat(List, ' ', Print),
    print(Print), print('\n').


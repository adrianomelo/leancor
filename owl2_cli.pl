:- [owl2_leancop].
:- [leancop21_swi].

ore :-
    current_prolog_flag(argv, Argv),
    print_start(Argv),
    activity(Argv),
    print_end(Argv).

print_start([Operation, OntologyFile, _]) :-
    list_p(['Started', Operation, 'on', OntologyFile]).

print_end([Operation, OntologyFile, _]) :-
    list_p(['Completed', Operation, 'on', OntologyFile]).

activity([consistency, OntologyFile, Output]) :-
    consistency(OntologyFile, Output).

activity([classification, OntologyFile, Output]) :-
    classify(OntologyFile, OperationTime, Output),
    list_p(['Operation Time:', OperationTime]).

activity([realisation, OntologyFile, Output]) :-
    realisation(OntologyFile, Output).

list_p(List) :-
    atomic_list_concat(List, ' ', Print),
    print(Print), print('\n').

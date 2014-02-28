
ore :-
    current_prolog_flag(argv, Argv),
    printarg(Argv).

printarg([]).
printarg([A|Rest]) :-
    print(A), print('\n\n\n'),
    printarg(Rest).


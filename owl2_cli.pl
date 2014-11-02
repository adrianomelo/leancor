:- [owl2_leancop].
:- [leancop21_swi].

:- dynamic(file/2).

ore :-
    current_prolog_flag(argv, Argv),
    append(_, [O,In,Out], Argv),
    run(O, In, Out).

run(Operation, Input, Output) :-
    writef('Started %p on %p\n', [Operation, Input]),
    assert_files(Input, Output),
    activity(Operation, OperationTime),
    writef('Operation Time: %p\n', [OperationTime]),
    writef('Completed %p on %p\n', [Operation, Input]).

activity(consistency, OperationTime) :-
    consistency(OperationTime).

activity(classification, OperationTime) :-
    classify(OperationTime).

activity(realisation, OperationTime) :-
    realisation(OperationTime).

assert_files(Input, Output) :-
    atom_concat(Output, '_debug', Debug),
    atom_concat(Output, '_err', Error),
    atom_concat(Output, '_info', Info),
    assert(file(input, Input)),
    assert(file(output, Output)),
    assert(file(debug, Debug)),
    assert(file(error, Error)),
    assert(file(info, Info)).

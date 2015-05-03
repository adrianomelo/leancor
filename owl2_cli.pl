:- [owl2_leancop].
:- [leancop21_swi].

:- dynamic(file/2).

main :-
    current_prolog_flag(argv, Argv),
    run(Argv).

run(Args) :-
    member(time, Args),
    append(_, [O,In,Out], Args),
    time(O, In, Out), !.

run(Args) :-
    append(_, [O,In,Out], Args),
    ore(O, In, Out), !.

time(Operation, Input, Output) :-
    assert_files(Input, Output),
    activity(Operation, OperationTime),
    writef("%p\n", [OperationTime]).

ore(Operation, Input, Output) :-
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
    assert(file(consistency, Output)),
    assert(file(debug, Debug)),
    assert(file(error, Error)),
    assert(file(info, Info)).

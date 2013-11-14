skolem_function(Function) :-
    skolemcounter(Counter),
    retractall(skolemcounter(_)),
    NewCounter is Counter+1,
    assert(skolemcounter(NewCounter)),
    atomic_list_concat([f, NewCounter], Function).

skolem_apply(Function, A, Fa) :-
    Fa=..[Function, A].

skolem_apply_list([], A, A).
skolem_apply_list([Function|List], A, Fa) :-
    skolem_apply(Function, A, Fna),
    skolem_apply_list(List, Fna, Fa).

skolem_clear :-
    retractall(skolemcounter(_)),
    assert(skolemcounter(0)).

:- dynamic(skolemcounter/1).
:- assert(skolemcounter(0)).
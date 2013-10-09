:- [owl2_clausal].
:- begin_tests(nested).

test(nested0) :-
    Input  = [a, -b],
    nested(Input, Output),
    Output == [[a, -b]].

test(nested1) :-
    Input  = [[[a], [[[b], [c]]]], -d],
    nested(Input, Output),
    Output == [[a, -d], [b, -d], [c, -d]].

test(nested2) :-
    Input  = [[[a], [[[b], [[[c], [d]]]]]], -e],
    nested(Input, Output),
    Output == [[a, -e], [b, -e], [c, -e], [d, -e]].

test(nested3) :-
    Input  = [a, [[b], [c]], -d],
    nested(Input, Output),
    Output == [[a, b, -d], [a, c, -d]].

test(nested4) :-
    Input  = [[[a], [b, c]], -d],
    nested(Input, Output),
    Output == [[a, -d], [b, c, -d]].

test(nested5) :-
    Input  = [[[a], [b, c, d]], -e],
    nested(Input, Output),
    Output == [[a, -e], [b, c, d, -e]].

test(nested6) :-
    % a -> c and g and (d or e or f) and (i or h)
    Input = [a, [[-c], [[[-g], [[[-d, -e, -f], [-i, -h]]]]]]],
    nested(Input, Output),
    Output == [[a, -c], [a, -g], [a, -d, -e, -f], [a, -i, -h]].

test(nested7) :-
    % c and g and (d or e or f) and (i or h) -> a
    Input = [c, g, [[d], [[[e], [f]]]], [[i], [h]], -a],
    nested(Input, Output),
    Output == [[c, g, d, i, -a], [c, g, e, i, -a], [c, g, f, i, -a], [c, g, d, h, -a], [c, g, e, h, -a], [c, g, f, h, -a]].

test(nested8) :-
    % g -> a and b and e and f and (d or c)
    Input = [g, [[-a], [[[-b], [[[-e], [[[-f], [-d, -c]]]]]]]]],
    nested(Input, Output),
    Output == [[g, -a], [g, -b], [g, -e], [g, -f], [g, -d, -c]].

test(nested9) :-
    % a and b and e and f and (d or c) -> g
    Input = [a, b, e, f, [[d], [c]], -g],
    nested(Input, Output),
    Output == [[a, b, e, f, d, -g], [a, b, e, f, c, -g]].

test(nested10) :-
    % a1 -> a or b or c or d and e or f or g or h
    Input = [a1, [[-e, -f, -g, -h], [-a, -b, -c, -d]]],
    nested(Input, Output),
    Output == [[a1, -e, -f, -g, -h], [a1, -a, -b, -c, -d]].

test(nested11) :-
    % a or b or c or d and e or f or g or h -> a1
    Input = [[[e], [[[f], [[[g], [h]]]]]], [[a], [[[b], [[[c], [d]]]]]], -a1],
    nested(Input, Output),
    Output == [[e, a, -a1], [e, b, -a1], [e, c, -a1], [e, d, -a1],
               [f, a, -a1], [f, b, -a1], [f, c, -a1], [f, d, -a1],
               [g, a, -a1], [g, b, -a1], [g, c, -a1], [g, d, -a1],
               [h, a, -a1], [h, b, -a1], [h, c, -a1], [h, d, -a1]].

test(listclausules1) :-
    Input = [[a], [[[b], [c]]]],
    list_clausules(Input, Output),
    Output == [[a], [b], [c]].

test(listclausules2) :-
    Input = [[a], [[[b], [[[c], [d]]]]]],
    list_clausules(Input, Output),
    Output == [[a], [b], [c], [d]].

test(listclausules3) :-
    Input = [[a], [b, c]],
    list_clausules(Input, Output),
    Output == [[a], [b, c]].

test(listclausules4) :-
    Input = [[-c], [[[-g], [[[-d, -e, -f], [-i, -h]]]]]],
    list_clausules(Input, Output),
    Output == [[-c], [-g], [-d, -e, -f], [-i, -h]].

test(listclausules5) :-
    Input = [[-e, -f, -g, -h], [-a, -b, -c, -d]],
    list_clausules(Input, Output),
    Output == [[-e, -f, -g, -h], [-a, -b, -c, -d]].

test(getnested1) :-
    Clausules = [a,b,c,[d,e],f],
    get_nested(Clausules, Nested, NotNested),
    Nested == [[d, e]],
    NotNested == [a, b, c, f].

test(getnested2) :-
    Clausules = [[a, b], -c],
    get_nested(Clausules, Nested, NotNested),
    Nested == [[a, b]],
    NotNested == [-c].

test(getnested3) :-
    Clausules = [a,b,c,[d,e],f,[g,h,i]],
    get_nested(Clausules, Nested, NotNested),
    Nested == [[d,e],[g,h,i]],
    NotNested == [a,b,c,f].

test(getnested4) :-
    Clausules = [a,b,c,[d,e],f,[g,[h,i]]],
    get_nested(Clausules, Nested, NotNested),
    Nested == [[d,e],[g,[h,i]]],
    NotNested == [a,b,c,f].

test(combine_clausules1) :-
    HeadNested = [drancestor, dr],
    NotNested  = [-drancestor],
    combine_clausules(HeadNested, NotNested, Matrix),
    Matrix == [[drancestor, -drancestor], [dr, -drancestor]].

:- end_tests(nested).
:- run_tests.

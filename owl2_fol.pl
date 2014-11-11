:- [def_mm].
:- [owl2_operators].

to_fol(A equivalent B, (all New: ((Fa) <=> (Fb)))) :-
    to_fol(New, A, Fa), to_fol(New, B, Fb).

to_fol(A is_a B, (all New: ((Fa) => (Fb)))) :-
    to_fol(New, A, Fa), to_fol(New, B, Fb).

to_fol(A disjoint B, (all New: (Fa => ~Fb))) :-
    to_fol(New, A, Fa),
    to_fol(New, B, Fb).

to_fol(A assert B, F) :-
    to_fol(B, A, F).

to_fol(P assert_p [A, B], F) :-
    F=..[P,A,B].

to_fol(P negative_p [A, B], ~F) :-
    F=..[P,A,B].

to_fol(A same B, A = B).

to_fol(A domain B, (all NewA: (Fa => Fb))) :-
    Fa=..[A,NewA,_],
    to_fol(NewA, B, Fb).

to_fol(A range B, (all NewA: (Fa => Fb))) :-
    Fa=..[A,_,NewA],
    to_fol(NewA, B, Fb).

to_fol(A subproperty B, (all X: (all Y: (Fa => Fb)))) :-
    Fa=..[A,X,Y],
    Fb=..[B,X,Y].

to_fol(A equivalent_p B, (all X: (all Y: (Fa <=> Fb)))) :-
    Fa=..[A,X,Y],
    Fb=..[B,X,Y]. 

to_fol(A disjoint_p B, (all X: (all Y: (Fa <=> ~Fb)))) :-
    Fa=..[A,X,Y],
    Fb=..[B,X,Y].

to_fol(A inverse B, (all X: (all Y: (Fa => Fb)))) :-
    Fa=..[A,X,Y],
    Fb=..[B,Y,X].

to_fol(symmetric A, (all X: (all Y: (Fa <=> Fb)))) :-
    Fa=..[A,X,Y],
    Fb=..[A,Y,X].

to_fol(asymmetric A, (all X: (all Y: (Fa => ~Fb)))) :-
    Fa=..[A,X,Y],
    Fb=..[A,Y,X].

to_fol(transitive A, (all X: (all Y: (all Z: (Fa, Fb => Fc))))) :-
    Fa=..[A,X,Y],
    Fb=..[A,Y,Z],
    Fc=..[A,X,Z].

to_fol(functional A, (all X: (all Y: (all Z: ((Fa, Fb) => (Y=Z)))))) :-
    Fa=..[A,X,Y],
    Fb=..[A,X,Z].

to_fol(inverse_functional A, (all X: (all Y: (all Z: ((Fa, Fb) => (X=Z)))))) :-
    Fa=..[A,X,Y],
    Fb=..[A,Z,Y].

to_fol(reflexive A, (all X: Fa)) :-
    Fa=..[A,X,X].

to_fol(irreflexive A, (all X: ~Fa)) :-
    Fa=..[A,X,X].

to_fol(_ disjoint_union _, []) :- !. % FIXME

to_fol(individual _, []) :- !.
to_fol(class _, []) :- !.                   % does not generate clauses
to_fol(property _, []) :- !.                % does not generate clauses

% individual
to_fol(A, A) :-
    atom(A).

% equality
to_fol((A different B), (A=Fb)) :-               % WRONG
    to_fol(B, Fb).

% class expessions
to_fol(New, A and B, (Fa , Fb)) :-
    to_fol(New, A, Fa),
    to_fol(New, B, Fb).

to_fol(New, A or B, (Fa ; Fb)) :-
    to_fol(New, A, Fa),
    to_fol(New, B, Fb).

to_fol(New, A some B, (ex NewNew: (Fa, Fb))) :-
    to_fol(New, NewNew, A, Fa),
    to_fol(NewNew, B, Fb).

to_fol(New, A any B, (all NewNew: (Fa => Fb))) :-
    to_fol(New, NewNew, A, Fa),
    to_fol(NewNew, B, Fb).

to_fol(New, not A, (~(Fa))) :-
    to_fol(New, A, Fa).

to_fol(New, A one B, (New = A; New = B)) :-
    atom(A), atom(B).

to_fol(New, A one B, (New = A; Fb)) :-
    to_fol(New, B, Fb).

to_fol(New, A value B, (Fa)) :-
    Fa=..[A,New,B].

to_fol(New, self A, (Fa)) :-
    Fa=..[A,New,New].

to_fol(New, [Number, P] min A, unknow).
to_fol(New, Number min A, unknow).
to_fol(New, [Number, P] min_d A, unknow).
to_fol(New, Number min_d A, unknow).
to_fol(New, [Number, P] max A, unknow).
to_fol(New, Number max A, unknow).
to_fol(New, [Number, P] max_d A, unknow).
to_fol(New, Number max_d A, unknow).
to_fol(New, [Number, P] exact A, unknow).
to_fol(New, Number exact A, unknow).
to_fol(New, [Number, P] exact_d A, unknow).
to_fol(New, Number exact_d A, unknow).

to_fol(New, A, (Fa)) :-
    Fa=..[A,New].

% to_fol with 4 args
to_fol(New, NewNew, A, (Fa)) :-
    Fa=..[A,New, NewNew].


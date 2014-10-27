
% assertions
:- op(80, xfx, [assert,
                assert_p]).                     % data and object property assert

% disjointness
:- op(90, xfx, [different,                      % different individuals
                disjoint_union,
                disjoint,                       % disjoint classes
                disjoint_o,
                disjoint_d]).

% equivalence
:- op(100, xfx, [is_a,
                subproperty,
                same,                           % same individuals
                equivalent,                     % equivalent classes
                equivalent_p]).                 % equivalent object and data properties

% declarations
:- op(110, fx, [class,
                individual,
                property,
                decl_datatype]).

% property
:- op(130, xfx, [inverse,
                domain,                         % object and data
                range,
                negative_p]).                        % object and data

:- op(130, fx, [symmetric,
                asymmetric,
                transitive,
                reflexive,
                irreflexive,
                functional,
                inverse_functional]).

:- op(140, xfx, haskey).

:- op(150, xfx, [any, some, and, or, max, min, exact,
                 one, inverse, value, self,
                 any_d, some_d, and_d, or_d, max_d,
                 min_d, exact_d, one_d, value_d]).

:- op(150, fx, [one, not, self,
                one_d]).


% assertions
:- op(80, xfx, [class_assert,
                property_assert_o,
                property_assert_d]).

% disjointness
:- op(90, xfx, [different_individuals,
                disjoint_union,
                disjoint_classes,
                disjoint_o,
                disjoint_d]).

% equivalence
:- op(100, xfx, [is_a,
                subproperty,
                same_as,
                eq_classes,
                eq_properties_o,
                eq_properties_d]).

% declarations
:- op(110, fx, [decl_class,
                decl_object_property,
                decl_data_property,
                decl_datatype]).

% property
:- op(130, xfx, [inverse,
                domain,                         % object and data
                range]).                        % object and data

:- op(130, fx, [symmetric,
                asymmetric,
                transitive,
                reflexive,
                irreflexive,
                functional,
                inverse_functional,
                negative]).

:- op(140, fx, has_key).

:- op(150, xfx, [any, some, and, or, max, min, exact,
                 one, inverse, value, self,
                 any_d, some_d, and_d, or_d, max_d,
                 min_d, exact_d, one_d, value_d]).

:- op(150, fx, [one, not, self,
                one_d]).

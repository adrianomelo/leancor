leancor
=======
A fork of leanCoR that aims description logics reasoning.

Usage
=======
Check consistency (not supported yet)

```
./leancor consistency tests/ontologies/examples/ontology_travel.owl ../travel-consistency.csv
```

Classify ontology

```
./leancor classification tests/ontologies/examples/ontology_travel.owl ../travel.owl
```

Check satisfiability of concept names (not supported yet)

```
./leancor sat test/pizza.owl test/output/pizza-sat.csv http://www.co-ode.org/ontologies/pizza/pizza.owl#SloppyGiuseppe
./leancor sat test/pizza.owl test/output/pizza-sat.csv http://www.co-ode.org/ontologies/pizza/pizza.owl#IceCream
```

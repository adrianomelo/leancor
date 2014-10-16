leancor
=======
A fork of leanCoR that aims description logics reasoning.

Usage
=======
Check consistency
./leancor consistency test/pizza.owl test/output/pizza-consistency.csv

Classify ontology
./leancor classification test/pizza.owl test/output/pizza-classification.owl

Check satisfiability of concept names
./leancor sat test/pizza.owl test/output/pizza-sat.csv http://www.co-ode.org/ontologies/pizza/pizza.owl#SloppyGiuseppe
./leancor sat test/pizza.owl test/output/pizza-sat.csv http://www.co-ode.org/ontologies/pizza/pizza.owl#IceCream


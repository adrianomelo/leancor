
write_classification_output_file :-
    file(output, FileName),
    open(FileName, write, File),
    current_output(Current),
    set_output(File),
    write_classification_output,
    close(File),
    set_output(Current).
 
write_classification_output :-
    file(output, FileName),
    prefix('',Uri),
    writef('Prefix(:=<%p#>)\n\
Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\n\
Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)\n\
Prefix(xml:=<http://www.w3.org/XML/1998/namespace>)\n\
Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\n\
Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n\n\
Ontology(<file:%p>\n\n', [Uri, FileName]),
    output_axioms,
    writef(')\n').
write_classification_output :-
    file(output, FileName),
    writef('Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\n\
Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)\n\
Prefix(xml:=<http://www.w3.org/XML/1998/namespace>)\n\
Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\n\
Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n\n\
Ontology(<file:%p>\n\n', [FileName]),
    output_axioms,
    writef(')\n').

output_axioms :-
    prefix('',Uri),
    forall((subclassof(A,B), not((subclassof(A,C), subclassof(C,B)))), writef('SubClassOf(<%p#%p> <%p#%p>)\n', [Uri, A, Uri, B])).
output_axioms :-
    forall((subclassof(A,B), not((subclassof(A,C), subclassof(C,B)))), writef('SubClassOf(%p %p)\n', [A, B])).

% File with debug information
write_debug(Axioms, Fol, Matrix) :-
    file(debug, FileName),
    open(FileName, write, File),
    current_output(Current),
    set_output(File),
    writef('------- Axioms --------\n'),
    write_debug_axioms(Axioms),
    writef('\n\n------- Formula --------\n'),
    write_debug_formula(Fol),
    writef('\n\n------- Matrix -------\n'),
    write_debug_matrix(Matrix),
    close(File),
    set_output(Current).

write_debug_axioms([]).
write_debug_axioms([Head|Axioms]) :-
    writef('%p\n', [Head]),
    write_debug_axioms(Axioms).

write_debug_formula((A,B)) :-
    writef('%p\n', [A]),
    write_debug_formula(B).
write_debug_formula(A) :-
    writef('%p\n', [A]).

write_debug_matrix([]).
write_debug_matrix([Head|Matrix]) :-
    writef('%p\n', [Head]),
    write_debug_matrix(Matrix).

write_debug_tuple(Name, Value) :-
    file(info, FileName),
    open(FileName, append, File),
    current_output(Current),
    set_output(File),
    writef('%p: %p\n', [Name, Value]),
    close(File),
    set_output(Current).

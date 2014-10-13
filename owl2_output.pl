
write_classification_output_file(FileName) :-
    open(FileName, write, File),
    current_output(Current),
    set_output(File),
    write_classification_output(FileName),
    close(File),
    set_output(Current).
 
write_classification_output(URI) :-
    writef('Prefix(owl:=<http://www.w3.org/2002/07/owl#>)\n\
Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)\n\
Prefix(xml:=<http://www.w3.org/XML/1998/namespace>)\n\
Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\n\
Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)\n\n\n\
Ontology(<file:%p>\n\n', [URI]),
    output_axioms,
    writef(')\n').

output_axioms :-
    prefix('',Uri),
    forall(subclassof(A,B,i), writef('SubClassOf(<%p#%p> <%p#%p>)\n', [Uri, A, Uri, B])).


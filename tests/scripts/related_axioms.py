
import re
import os
import sys

from os.path import isfile

root='/home/adr/tests/ore-competition'
report_dir="$root/data/responses/report"
ontology_path='ore2014/files'
ontology_dir="$root/data/ontologies/$ontology_path"

def find_uris(string):
	return re.findall('<[^>]*>', string)

def find_axiom_related_with(ontology, uri):
	lines = ontology.split('\n')
	axioms = []
	for line in lines:
		if line.find(uri) > 0:
			axioms.append(line)

	return axioms

def process_files(diff_file, ontology_file):
	uris_axioms = {}
	uris_uris = {}

	errors = open(diff_file, 'r').readlines()
	errors = [e for e in errors if e[0] == '+' or e[0] == '-']
	pairs = map(find_uris, errors)

	ontology = open(ontology_file, 'r').read()
	all_uris = find_uris(ontology)

	for uri in all_uris:
		if not uris_axioms.has_key(uri):
			uris_axioms[uri] = find_axiom_related_with(ontology, uri)

	for i in xrange(len(pairs)):
		pair = pairs[i]

		if (len(pair) != 2):
			continue

		print "-" * 100
		print errors[i]
		print ('Concepts: %s' % ' and '.join(pairs[i]))
		print 'Related axioms:\n'

		axioms = uris_axioms[pair[0]] + uris_axioms[pair[1]]
		print '\n'.join(axioms)

if __name__ == '__main__':
	process_files(sys.argv[1], sys.argv[2])
	

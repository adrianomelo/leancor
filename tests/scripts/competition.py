
from os import listdir
import re
import sets
import json

ontologies_dir='/home/adr/tests/ore-competition/data/ontologies/ore2014/files/'
newcompetitions_dir='/home/adr/competitions/'

count = 0
categories = {}
ontologies = {}

def process_file(ontology):
	content = file(ontologies_dir + ontology, 'r').read()
	matches = re.findall('[A-Z][A-Za-z]+\(', content)
	matches = matches + re.findall('(Data[A-Za-z]+\(|Object[a-zA-Z]+\(|HasKey)', content)
	matches = [a[:-1] for a in matches]
	axioms = sets.ImmutableSet(matches)
	key = hash(axioms)
	categories[key] = axioms
	if (ontologies.has_key(key)):
		ontologies[key].append(ontology)
	else:
		ontologies[key] = [ontology,]


def save_filelist(key, filelist, ext='.filelist'):
	filelist_arq = file(newcompetitions_dir + key + ext, 'w')
	filelist_arq.write('\n'.join(filelist))
	filelist_arq.close()

if __name__ == "__main__":
	for f in listdir(ontologies_dir):
		if count > 200:
			break
		#count += 1
		if (f[-3:] == 'owl'):
			process_file(f)
	
	output = []
	for (key,ontos) in ontologies.items():
		output.append({
			'key': str(key),
			'axioms': len(categories[key]),
			'ontologies': len(ontologies[key])
		})
		
		save_filelist(str(key), ontos)
		save_filelist(str(key), categories[key], ext='.axiomlist')

	json_output = {'output': output}
	print json.dumps(json_output, indent=4)

	out = file(newcompetitions_dir + '_index.json', 'w')
	out.write(json.dumps(json_output, indent=4))
	out.close()

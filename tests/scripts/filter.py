import csv
import hashlib

onserver = '/Users/amelo/forest/ufpe/list.txt'
ontologies = '/Users/amelo/forest/ufpe/ore-comptetition/data/ontologies/ore2014/dl/classification/metadata.csv'
competitions = '/Users/amelo/forest/ufpe/competitions/'
template = '''
# axioms: {1}
CompetitionName	OWL Classification {0}

OutputPathName	classification-{0}

Reasoners	chainsaw-linux	fact++-linux	hermit-linux	jfact-linux	konclude-linux	morehermit-linux	treasoner-linux	trowl-linux	leancor

QueryPathFilterString	classification/ore2014

QuerySortingFilePathString	RELATIVETOCOMPETITIONSDIRECTORY	{2}

StartingDateTime	2014-07-18T10:35:00.000+02:00

EndingDateTime	2014-07-18T17:35:00.000+02:00

AllowProcessingOnlyBetweenDateTimeInterval	TRUE

ExecutionTimeout	180000

ProcessingTimeout	150000'''

groups = {}

with open(ontologies, 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
    	uri = row[-6]
    	if uri == 'filename':
    		continue

    	axioms = row[14].split(' ')
    	axioms = filter(None, axioms)
    	axioms.sort()

    	key = ' '.join(axioms)
    	
    	if (groups.has_key(key)):
    		groups[key].append(uri)
    	else:
    		groups[key] = [uri,]


keys = groups.keys()
keys.sort()

allowed = open(onserver, 'r').read()
allowed = allowed.split('\n')
allowed = frozenset(allowed)

#print groups

for i in xrange(len(keys)):
	key = keys[i]
	name_prefix = ('%03d' % len(key.split(' '))) + '-' + ('%04d' % len(groups[key])) + '-' + str(i)
	name_filelist = competitions +  name_prefix + '.filelist'
	name_competition = competitions + name_prefix + '.dat'

	allowed_ontologies = filter(lambda x: x in allowed, groups[key])

	filelist = open(name_filelist, 'w')
	filelist.write('\n'.join(allowed_ontologies))
	filelist.close()

	competition = open(name_competition, 'w')
	competition.write(template.format(i, key, name_prefix + '.filelist'))
	competition.close()

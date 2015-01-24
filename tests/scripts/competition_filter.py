import json

competitions_file='/home/adr/competitions/_index.json'

data = json.loads(open(competitions_file, 'r').read())

print [a for a in data['output'] if a['ontologies'] > 100]
 


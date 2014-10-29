
IFS=$'\n'
FOLDER=tests/ontologies/ore2014/[0-9]*
for file in `ls ${FOLDER}`
do
	if [ ! -f "tests/output/classification-$(basename $file)_err" ]
	then
		echo "$(date) - ${file}";
		ulimit -t 1
		./leancor classification "${file}" tests/output/classification-$(basename "$file")
	else
		echo "skipping tests/output/classification-$(basename $file)"
	fi
done

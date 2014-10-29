
#IFS=$'\n'
#FOLDER=tests/ontologies/ore2014/[0-9]*
FILES=tests/ontologies/dataset/el/classification/fileorder.txt
FILES_DIR=tests/ontologies/dataset/files/
OUTPUT_DIR=tests/output

FILE_LIST=`cat $FILES`

for file in $FILE_LIST
do
	if [ ! -f "${OUTPUT_DIR}/classification-${file}_err" ]
	then
		echo "$(date) - ${file}";
		ulimit -t 360;
		./leancor classification "${FILES_DIR}/${file}" "${OUTPUT_DIR}/classification-${file}";
	else
		echo "skipping tests/output/classification-$file"
	fi
done

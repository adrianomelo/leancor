
IFS=$'\n'
FOLDER=tests/ontologies/el-less-than-100kb/*
for file in `ls ${FOLDER}`
do
	echo "$(date) - ${file}";
    ./leancor classification "${file}" tests/output/classification-$(basename "$file")
done

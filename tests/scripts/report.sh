#!/bin/bash
output_path_name='classification-1514'
file_list_name='003-0194-1514.filelist'
ontology_path='ore2014/files'

winner_reasoner='trowl-linux'
leancor_reasoner='leancor'

root='/home/adr/tests/ore-competition'
filelist="$root/data/competitions/$file_list_name"
ontology_dir="$root/data/ontologies/$ontology_path"
expectations_dir="$root/data/expectations/relative/classification/ore2014/el/files"
expected_dir="$root/data/responses/competition-evaluations/$output_path_name/reasoners-responses/$winner_reasoner/relative/$ontology_path"
responses_dir="$root/data/responses/competition-evaluations/$output_path_name/reasoners-responses/$leancor_reasoner/relative/$ontology_path"
report_dir="$root/data/responses/report"

files=`cat $filelist`
server=`hostname`
server_addr="http://$server.soft.cs.uni-potsdam.de:8080/data/responses/report"

timeouts=0
correct=0
unexpected=0

for file in $files
do
        #echo "$expected_dir/$file/query-result-data.owl"
        if [ -a "$responses_dir/$file/query-result-data.owl" ]
        then
                if diff "$expected_dir/$file/query-result-data.owl.hash" "$responses_dir/$file/query-result-data.owl.hash" > /dev/null
                then
                        correct=$(($correct+1))
                else
                        expected=`cat "$expected_dir/$file/query-result-data.owl" | grep SubClassOf | sort`
                        response=`cat "$responses_dir/$file/query-result-data.owl" | grep SubClassOf | sort`
                        echo "$expected" > /tmp/out1
                        echo "$response" > /tmp/out2

                        diff_file="$report_dir/$file.txt"
                        diff -u /tmp/out2 /tmp/out1 > $diff_file

                        report_file="$report_dir/$file.report.txt"
                        python "related-axioms.py $diff_file $ontology_dir/$file" > $report_file

                        adds=`cat $diff_file | grep '\+SubClassOf' | wc -l`
                        deletes=`cat $diff_file | grep '\-SubClassOf' | wc -l`

                        echo "We need $adds new axioms and $deletes less axioms, $server_addr/$file.report.txt"
                        unexpected=$(($unexpected+1))
                fi
        else
                timeouts=$(($timeouts+1))
        fi
done

echo "Correct outputs: $correct, timeouts: $timeouts, unexpected: $unexpected"
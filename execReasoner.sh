#!/bin/bash
# 
# OWL Reasoner Evaluation Workshop (ORE) 2013
# Example reasoner executor script
# Last updated: 27-Mar-13
# 
ulimit -t 180
cd /Users/amelo/forest/ufpe/ore-comptetition/data/reasoners/leancor/
#echo $*
swipl -O -q -G7g -T7g -L7g -f owl2_cli.pl -t halt. -g ore $* 2> $5_err

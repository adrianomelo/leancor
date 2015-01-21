#!/bin/bash

cd $(dirname $0)
timeoutsec=`expr $1 / 1000`
timeout $timeoutsec swipl -O -q -G7g -T7g -L7g -f owl2_cli.pl -t halt. -g main $*

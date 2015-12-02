#!/bin/bash
FILES=$1/*.txt.split
acedir=ace-0.9.22
for f in $FILES
do
	$acedir/ace -g $acedir/erg-1214-osx-0.9.22.dat -1Tf $f > $f\.mrs
done


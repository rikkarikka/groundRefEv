#!/bin/bash
FILES=$1/*.txt.split
for f in $FILES
do
	./ace_dir/ace -g ./ace_dir/erg-1214-osx-0.9.22.dat -1Tf $f > $f\.mrs
done


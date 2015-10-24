#!/bin/bash
FILES=problems/*.txt.split
for f in $FILES
do
	./ace_dir/ace -g ./ace_dir/erg-1212-osx-0.9.21.dat -1Tf $f > $f\.mrs
done


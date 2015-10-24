mkdir skipped;
while read p ; do mv $p skipped ; done < <(grep -l "SKIP" problems/*.mrs)

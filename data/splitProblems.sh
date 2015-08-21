i=0; while read p ; do echo $p > problems/$i\.txt ; let "i+=1";  done < wp.testData 
scala ./splitQuestion.scala

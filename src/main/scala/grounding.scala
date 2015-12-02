import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import java.io._
import libsvm.svm._
import breeze.linalg._
/*
object gl {
        //def REL_LIST= Source.fromFile("data/rel_list.txt").mkString.split("\n").distinct
        def REL_LIST= Source.fromFile("data/rel_list.reduced.txt").mkString.split("\n").distinct
        val probdir = "data/problems/"
        val dir = new File(probdir)
        val problems = dir.list filter (_.endsWith("mrs"))
        var answers = Source.fromFile("data/a.txt").mkString.split("\n") map (_.toFloat)
        var equations = Source.fromFile("data/eq.txt").mkString.split("\n")
        def close(a:Float,b:Float,thresh:Double=0.001): Boolean = {
            return Math.abs(a-b)<thresh
        }
        var m = new Word2Vec()
        m.load("data/smallvectors.bin")
        val ops = List("+","-","*","/","=")
}
*/
object grounding{
	val VERBOSE = true

	def parseSentence(input:String): List[MRS] = {
		var sent = input.split("SENT:")(1).split("\\[")(0).trim	
		var spl = input.split("RELS:")(1).split("HCONS")(0)
		val mrs = spl.split("\n") 
		def new_mrs(i:Int):MRS = new MRS(sent, mrs(i))
		var objs = List.tabulate(mrs.length){new_mrs}
		return objs
	}
	
	def parseStory(sentencesText:Array[String]): World = {


            var w = new World()
            for (x<-sentencesText) {
                    var objs = parseSentence(x)
                    w.update(objs)
                    w.sidx += 1
            }
       
            return w

	}


	def main(args:Array[String]) {
            val probdir = "data/problems/"
            val dir = new File(probdir)
            val problems = dir.list filter (_.endsWith("mrs")) 
            var answers = Source.fromFile("data/a.txt").mkString.split("\n") map (_.toDouble)
            var rels = new ListBuffer[String]
            
            problems foreach { x => 
                println(f"Constructing World from file: $x%s")

                //this reads the MRSes of each problem
		var fi = Source.fromFile(probdir+x)
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()

                // create a World from MRSes
                val w = parseStory(sentences)

                // get rels
                w.numbers foreach {x=>x.relations foreach {y=>rels += y.r}}
                w.variables foreach {x=>x._2.relations foreach {y=>rels += y.r}}

                /*
                //serialize 
                var pidx = x.split("\\.")(0)
                val oos = new ObjectOutputStream(new FileOutputStream(probdir+pidx+".world"))
                oos.writeObject(w)
                oos.close()

                //this gets the associated answer
                var a = answers(pidx.toInt)


                // this writes the ILP input file and
                var types = w.numberEntities map (e=>e.n) distinct 
                var constantString = "constants : " 

                var otString = "objtypes : "
                types foreach { x => otString += x + " "}

                var cOrUkString = "constantOrUnknownType : "
                w.numbers foreach {e =>
                    constantString += e.card.toString + " "
                    cOrUkString += types.indexOf(e.n).toString + " "
                }
                if (w.variables.toList.length > 0) {
                    cOrUkString += types.indexOf((w.variables map (_._2)).toList(0).n).toString
                } else {
                    otString += "UK"
                    cOrUkString += types.length.toString
                }

                val ilpOutFi = new PrintWriter("data/ILPdir/q"+pidx+".ilp.txt")
                ilpOutFi.write(constantString+"\n")
                ilpOutFi.write("unknowns : x\n")
                ilpOutFi.write("operators : + - * / =\n")
                ilpOutFi.write(otString+"\n")
                ilpOutFi.write(cOrUkString+"\n")
                ilpOutFi.write("n : "+((w.numbers.toList.length*2+1).toString)+"\n")
                ilpOutFi.write("answer : "+a.toString+"\n")
                ilpOutFi.close
                */

            }

            //write out rels to file

            val relFi = new PrintWriter("data/predications.txt")
            rels.distinct filter {x=>x.charAt(0)!='\"'} foreach {x=>relFi.write(x+"\n")}
            relFi.close

	}	
}

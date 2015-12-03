import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import java.io._

object getNP{
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

                var NPS = w.EntityID map (_._2)
                NPS foreach {_.print()}

  
            }

	}	
}

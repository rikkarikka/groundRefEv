import scala.io.Source
import scala.collection.mutable.ListBuffer


class MRS(sent:String,specs:String){

	val wb = parseWB
	val word = sent substring(wb(0),wb(1))
	val rel = specs.split("\\[")(1).split("<")(0).trim
	val lbl = specs.split("LBL: ")(1).split(" ")(0)
	val args = parseArgs

	def trips(): List[(Any,Any)] = {
		var tmp = new ListBuffer[(Any,Any)]
		tmp += (("wbl",wb(0)),("wbr",wb(1)),("word",word),("rel",rel),("lbl",lbl))
		for (i <- 0 until 4) {
			if (i < args.length) tmp append (("ARG"+i.toString,args(i)))
			else tmp append (("ARG"+i.toString,"NONE"))
		}
		return tmp.toList
	}

		

	def parseWB(): Array[Int] = {
		//gets word boundary indices for this word
		val wordr = """\<[0-9]+:[0-9]+\>""".r
		val wordbound = wordr findFirstIn specs 
		return wordbound.mkString drop (1) dropRight (1) split (":") map (_.toInt)
	}

	def parseArgs(): List[String] = {
		var i = 0
		var a = new ListBuffer[String]
		while (specs contains "ARG"+i.toString) {
			a += specs.split("ARG"+i.toString+": ")(1).split(" ")(0)
			i+=1
		}
		return a.toList
	}

	def print(): Unit = {
		println(word)
		println(" -- ",rel)
		println(" -- ",lbl)
		println(" -- ",args)
	}
			
	
}



object parseQuestion {
	val VERBOSE = true

	def parseSentence(input:String): List[MRS] = {
		var sent = input.split("SENT:")(1).split("\\[")(0).trim	
		var spl = input.split("RELS:")(1).split("HCONS")(0)
		val mrs = spl.split("\n")
		// var objs = List[MRS]
		// var i = 0
		def new_mrs(i:Int):MRS = new MRS(sent, mrs(i))
		var objs = List.tabulate(mrs.length){new_mrs}

		// Get Entities and Events
		def entities():List[Int]= (for ((o,i) <- objs.zipWithIndex if o.args(0).startsWith("x")) yield i) 
		def events():List[Int]= (for ((o,i) <- objs.zipWithIndex if o.args(0).startsWith("e")) yield i) 

		//if (VERBOSE) entities.foreach(objs(_).print)
		//print(entities,events)
		return objs
	}
	


	def simpleCoref(story:List[List[MRS]]) {
		def named():List[(Int,MRS)] = {
			var tmp = new ListBuffer[(Int,MRS)]
			for ((s,j)<-story.zipWithIndex) {
				for ((o,i) <- s.zipWithIndex) {
					if (o.rel == "named_rel") {
						tmp.append((j*1000+o.wb(1),o))
					}
				}
			}
			return tmp.toList
		}
		print(story(0)(0).trips)
	
	}
					
		 

	def parseStory(sentencesText:Array[String]){
		var sentencesMRS = new ListBuffer[List[MRS]]
		for (x<-sentencesText) {
			sentencesMRS += parseSentence(x)
		}
		simpleCoref(sentencesMRS.toList)
	}
	
	def main(args:Array[String]) {
		var fi = Source.fromFile("data/problems/0.txt.split.mrs")
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()
		parseStory(sentences)
	}	
}

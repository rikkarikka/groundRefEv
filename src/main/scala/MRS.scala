import scala.collection.mutable.ListBuffer

class MRS(sent:String,specs:String){

	val wb = parseWB
	val word = sent substring(wb(0),wb(1))
	val rel = specs.split("\\[")(1).split("<")(0).trim
	val lbl = specs.split("LBL: ")(1).split(" ")(0)
	val args = parseArgs
        val carg = parseCarg
        var idx = "None"

        def parseCarg(): String = {
            var tmpcarg = "None"
            if (specs contains "CARG") {
                if (specs contains "CARG: *top*"){
                    tmpcarg = word
                } else {
                    tmpcarg = specs.split("CARG: \"")(1).split ("\"")(0)
                }
                if ((tmpcarg != word) & word(0).isDigit) tmpcarg = word
            }
            return tmpcarg
        }

	def trips(): Map[String,Any] = {
		var tmp = Map[String,Any]()
		tmp += (("wbl",wb(0)),("wbr",wb(1)),("word",word),("rel",rel),("lbl",lbl))
		for (i <- 0 until 4) {
                    if (i < args.length) tmp + ("ARG"+i.toString -> args(i))
                    else tmp + ("ARG"+i.toString -> "NONE")
		}
		return tmp
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




import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer
import java.io._
import libsvm.svm._


object parseQuestion {
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
            //w.EntityID foreach {case (k,v) => println(k + " --> " + v.mentions)}

            /*
            println("CARG Things:")
            for ((k,v) <- w.EntityID) {
                if (v.carg() != None) {
                    println(k,v.carg())
                    v.print()
                }
            }
            println("NUMBER Things:")
            for ((k,v)<-w.EntityID){
                if (v.card != None) {
                    println(k,v.card)
                }
            }
            */

            return w

	}
    
        def parseEquation(e:String): List[String] = {
            var r = """(\d*\.?\d+|[+\-\*\/\=])""".r
            return (r findAllIn e).toList //map (_.toFloat)
        }

        def solveProblem(w:World, a:Float, e:String): Int = {
            val numbers = w.numbers map (_.card)//w.EntityID map (_._2) filter (_.card != None) filter (_.card.asInstanceOf[String].charAt(0).isDigit) map (_.card.asInstanceOf[String].toFloat)
            val variables = w.variables()
            val eqValues = parseEquation(e)
            println(e)
            eqValues foreach println
            //numbers foreach println
            return 1
            
        }

	def ILPOut() {

            val probdir = "data/problems/"
            val dir = new File(probdir)
            val problems = dir.list filter (_.endsWith("mrs"))
            //produce training vectors for each problem
            problems foreach { x => 
                println(x)

                //this reads the MRSes of each problem
		var fi = Source.fromFile(probdir+x)
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()

                // create a World from MRSes of problem statement sentences
                val w = parseStory(sentences)


		    val numbers = w.numbers map (_.card)
		    val entities = w.numbers map (_.cannonicalName)
		    for(var i <- range(1,numbers.length) {
			print(i,numbers(i),entities(i))
			println()
		    }
	    }
	}
	    


        def rel_list(w:World) {
            val numbers = w.numbers //w.EntityID map (_._2) filter (_.card != None) filter (_.card.asInstanceOf[String].charAt(0).isDigit)
            numbers foreach {x=>x.relations foreach {y=>println(y.r)}}
        }


        def split(){
            val dir = new File("data/problems")
            var ifiles = dir.list filter (_.endsWith("mrs"))
            var files = Random.shuffle(ifiles.toList)
            var i = math.floor(files.length/5.0).toInt
            var test = files.take(i)
            var dev = files.drop(i).take(i)
            val ptest = new PrintWriter(new File("data/test.indexes"))
            test.foreach(x=>ptest.write(x+"\n"))
            ptest.close()
            val dtest = new PrintWriter(new File("data/dev.indexes"))
            dev.foreach(x=>dtest.write(x+"\n"))
            dtest.close()
        }
            
        def dev_results(m:libsvm.svm_model, w:World, a:Float, r:Array[String]): Int = {
            val veclist = w.vectorize_node(r)
            val n = w.numbers.map(_.card).toList
            var total = n(0)
            var i = 1
            veclist foreach { x =>
                var y = libsvm.svm.svm_predict(m,x)
                if (y==0) total -= n(i)
                if (y==1) total += n(i)
                i += 1
            }
            println(total,a)
            if (total == a) return 1
            else return 0
        }

        def train() {

            val probdir = "data/problems/"
            val dir = new File(probdir)

            //Dont train on the dev or test sets
            val dontrain = Source.fromFile("data/dev.indexes").mkString.split("\n").toList ++ Source.fromFile("data/test.indexes").mkString.split("\n").toList
            val problems = dir.list filter (_.endsWith("mrs")) filter { x => !(dontrain contains x)}

            //load answers and equations
            var answers = Source.fromFile("data/a.txt").mkString.split("\n") map (_.toFloat)
            var equations = Source.fromFile("data/eq.txt").mkString.split("\n")

            //produce training vectors for each problem
            problems foreach { x => 
                println(x)

                //this reads the MRSes of each problem
		var fi = Source.fromFile(probdir+x)
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()

                // create a World from MRSes of problem statement sentences
                val w = parseStory(sentences)

                //this gets the associated answer and equation
                var pidx = x.split("\\.")(0).toInt
                var a = answers(pidx)
                var e = equations(pidx)

                val s = solveProblem(w,a,e)
                //w.vectorize(REL_LIST,s)
                //rel_list(w)
            }

        }
	
	def main(args:Array[String]) {
            val probdir = "data/problems/"
            val REL_LIST= Source.fromFile("data/rel_list.txt").mkString.split("\n").distinct
            //REL_LIST foreach {x => println(x)}
            val dir = new File(probdir)
            var devFlag = false 
            var files = new Array[String](0)
            args(0) match {
		case "ILP" => {
		    ILPout()
		}
                case "split" => {
                    split()
                }
                case "train" => {
                    train()
                }
                case "test" => {
                    val test = Source.fromFile("data/test.indexes").mkString.split("\n").toList
                    files = dir.list filter (_.endsWith("mrs")) filter { x => (test contains x)}
                }
                case "dev" => {
                    val dev = Source.fromFile("data/dev.indexes").mkString.split("\n").toList
                    files = dir.list filter (_.endsWith("mrs")) filter { x => (dev contains x)}
                    devFlag = true
                }
                case "parse" => {
                    files = dir.list filter (_.endsWith("mrs"))
                }
                case _ => {
                    println("requrires test, train, or dev arg"); System.exit(1)
                }
            }



            /*
            //split(files);System.exit(0)
            var sum = 0
            var mm = 0
            var o = 0
            var missing = new ListBuffer[String]
            var answers = Source.fromFile("data/a.txt").mkString.split("\n") map (_.toFloat)
            var right = 0
            files foreach { x => 
                //try {
                println(x)
		var fi = Source.fromFile(probdir+x)
                var m = libsvm.svm.svm_load_model("data/simple.m")

                var pidx = x.split("\\.")(0).toInt
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()
                val w = parseStory(sentences)
                if (devFlag) {
                    right += dev_results(m,w,answers(pidx),REL_LIST)
                }
                else {
                val s = solveProblem(w,answers(pidx))
                s match {
                    case 1 => {sum+=1;}// w.EntityID filter (_._2.card != None) foreach {_._2.print()};}

                    case 0 => mm+=1
                    case -1 => {
                        o+=1; //print(x,"\n")
                        w.numbers
                    }
                }
                //w.vectorize(REL_LIST,s)
                //rel_list(w)
                }
            //} catch {case _ : Throwable => print(x)}
            }
            println(f"$sum%d ,$mm%d ,$o%d")
            println(right)
            println(files.length)
            */
	}	
}

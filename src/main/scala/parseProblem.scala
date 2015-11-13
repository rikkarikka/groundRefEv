import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import java.io._
import libsvm.svm._
import breeze.linalg._

object g {
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

        
        def cannonicalize(l:Float,r:Float, a:Float): String = {
            if (g.close(l+r,a)) return "+"
            if (g.close(l-r,a)||g.close(r-l,a)) return "-"
            if (g.close(l*r,a)) return "*"
            if (g.close(l/r,a)||g.close(r/l,a)) return "/"
            else return "fail"
        }


        def removeProblems() {

            val probdir = "data/problems/"
            val dir = new File(probdir)

            val problems = dir.list filter (_.endsWith("mrs")) 

            //load answers and equations
            var answers = Source.fromFile("data/a.txt").mkString.split("\n") map (_.toFloat)
            var equations = Source.fromFile("data/eq.txt").mkString.split("\n")

            //produce training vectors for each problem
            problems foreach { x => 

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

		try {
			val s = nonWorking(w,a,e)
            } catch {case _ : Throwable => println(x)}
            }
        }
	
        def nonWorking(w:World, a:Float, e:String): Int = {
            val eqValues = parseEquation(e) filter (x=>(x.charAt(0).isDigit)&&(x != "0.0"))
            var op = cannonicalize(eqValues(0).toFloat,eqValues(1).toFloat,a)
            if (eqValues.toList.length>2) op = "fail"
            /*
            val eqValues = parseEquation(e) map (x=>if (x=="x") "0.0" else x)
	    val op = eqValues filter (List("+","-","/","*") contains _)
	    val opIdx = eqValues.indexOf(op(0))
	    val lOperand = w.numbers filter (x => x.card == eqValues(opIdx-1).toFloat) 
	    val rOperand = w.numbers filter (x => x.card == eqValues(opIdx+1).toFloat)
	    */
            //if (op=="fail") {print(eqValues);print(a)}
	    val lOperand = w.numbers filter (x => x.card == eqValues(0).toFloat) 
	    val rOperand = w.numbers filter (x => x.card == eqValues(1).toFloat)
	    val vec = w.vector(g.REL_LIST,lOperand(0),rOperand(0))
	    var j=1
	    var opID = 0
	    op match {
		    case "+" => opID = 0
		    case "-" => opID = 1
		    case "*" => opID = 2
		    case "/" => opID = 3
	    }
            return 1
        }

    
        def parseEquation(e:String): List[String] = {
            var r = """(\d*\.?\d+|[x+\-\*\/\=])""".r
            return (r findAllIn e).toList map (_.toString)
        }


        def solveProblem(w:World, a:Float, e:String): Int = {
            //val numbers = w.numbers map (_.card)//w.EntityID map (_._2) filter (_.card != None) filter (_.card.asInstanceOf[String].charAt(0).isDigit) map (_.card.asInstanceOf[String].toFloat)
            val eqValues = w.numbers map (_.card) filter (x=>(x!=0.0)) //parseEquation(e) filter (x=>(x.charAt(0).isDigit)&&(x != "0.0"))
            var op = cannonicalize(eqValues(0).toFloat,eqValues(1).toFloat,a)
            /*
            val eqValues = parseEquation(e) map (x=>if (x=="x") "0.0" else x)
	    val op = eqValues filter (List("+","-","/","*") contains _)
	    val opIdx = eqValues.indexOf(op(0))
	    val lOperand = w.numbers filter (x => x.card == eqValues(opIdx-1).toFloat) 
	    val rOperand = w.numbers filter (x => x.card == eqValues(opIdx+1).toFloat)
	    */
            //if (op=="fail") {print(eqValues);print(a)}
	    val lOperand = w.numbers filter (x => x.card == eqValues(0).toFloat) 
	    val rOperand = w.numbers filter (x => x.card == eqValues(1).toFloat)
	    //val vec = w.vector(g.REL_LIST,lOperand(0),rOperand(0))
	    val vec = w.handmadeVector(g.m,g.REL_LIST,lOperand(0),rOperand(0))
	    var j=1
	    var opID = 0
	    //op(0) match {
	    op match {
		    case "+" => opID = 0
		    case "-" => opID = 1
		    case "*" => opID = 2
		    case "/" => opID = 3
	    }
            //if (opID > 1) return 1
	    print(opID.toString + " ")
            vec foreach {x => print(j.toString+":"+x.toString+" ");j+=1}
	    println()
	    return 1
        }

        def getVector(w:World, a:Float, e:String): DenseVector[Double] = {
            val eqValues = parseEquation(e) filter (x=>(x.charAt(0).isDigit)&&(x != "0.0"))
            var op = cannonicalize(eqValues(0).toFloat,eqValues(1).toFloat,a)
            if (eqValues.toList.length>2) op = "fail"
	    val lOperand = w.numbers filter (x => x.card == eqValues(0).toFloat) 
	    val rOperand = w.numbers filter (x => x.card == eqValues(1).toFloat)
	    val vec = w.bVector(g.REL_LIST,lOperand(0),rOperand(0))
            return vec
        }

        def getOp(w:World, a:Float, e:String): Int = {
            val eqValues = parseEquation(e) filter (x=>(x.charAt(0).isDigit)&&(x != "0.0"))
            var op = cannonicalize(eqValues(0).toFloat,eqValues(1).toFloat,a)
	    var opID = 0
            op match {
                    case "+" => opID = 0
                    case "-" => opID = 1
                    case "*" => opID = 2
                    case "/" => opID = 3
            }
            return opID
        }


        def doPCA() {
            val probdir = "data/problems/"
            val dir = new File(probdir)

            //Dont train on the dev or test sets
            val dontrain = Source.fromFile("data/dev.indexes").mkString.split("\n").toList ++ Source.fromFile("data/test.indexes").mkString.split("\n").toList
            val problems = dir.list filter (_.endsWith("mrs")) filter { x => !(dontrain contains x)}

            //load answers and equations
            var answers = Source.fromFile("data/a.txt").mkString.split("\n") map (_.toFloat)
            var equations = Source.fromFile("data/eq.txt").mkString.split("\n")

            var matrix = DenseMatrix.zeros[Double](g.REL_LIST.length*2,problems.length)
            var i = 0
            var ops = new ListBuffer[Int]
            //produce training vectors for each problem
            problems foreach { x => 

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

                val v = getVector(w,a,e)
                ops += getOp(w,a,e)
                matrix(::, i) := v
                i+=1
            }
            var PCA = princomp(matrix.t)
            var FirstNine = PCA.scores(::,0 to 99)
            i = 0
            while (i < problems.length) {
                var j = 0
                print(ops(i).toString + " ")
                0 to 99 foreach {x => print(j.toString+":"+FirstNine(i,x).toString+" ");j+=1}
                println()
                i += 1
            }
        }


	def ILPout() {

            val probdir = "data/problems/"
            val dir = new File(probdir)
            val problems = dir.list filter (_.endsWith("mrs")) // filter (_ contains "146")
            //produce training vectors for each problem
            problems foreach { x => 
                println(x)

                //this reads the MRSes of each problem
		var fi = Source.fromFile(probdir+x)
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()

                // create a World from MRSes of problem statement sentences
                val w = parseStory(sentences)


		//w.EntityID foreach {x => x._2.print()}
		    val numbers = "constants : " + (w.numbers map (x => "%s " format x.card.toString)).mkString //_.card) toList

		    //val entities = w.numbers map (_.mentions(0)._1) toList
		    var i = 0
		    println(numbers)
		    /*
		    while (i < numbers.length) {
			print(i);print(numbers(i));print(entities(i))
			println()
			i+=1
		    }
		    */
	    }
	}
	    

        def listRels() {
            var rels = new ListBuffer[String]
            g.problems foreach { x => 
                var fi = Source.fromFile(g.probdir+x)
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()
                val w = parseStory(sentences)
                w.numbers foreach {x=>x.relations foreach {y=>rels += y.r}}
                w.variables foreach {x=>x._2.relations foreach {y=>rels += y.r}}
            }
            rels.distinct filter {x=>x.charAt(0)!='\"'} foreach println
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

        def dev() {
            val dev = Source.fromFile("data/dev.indexes").mkString.split("\n").toList 
            val problems = g.problems filter { x => (dev contains x)}
            val m = libsvm.svm.svm_load_model("data/simple.m")
            var right = 0
            problems foreach { x =>
                var fi = Source.fromFile(g.probdir+x)

                var pidx = x.split("\\.")(0).toInt
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()
                val w = parseStory(sentences)
                right += dev_results(m,w,g.answers(pidx),g.REL_LIST)
            }
            println(f"right $right%d ; total ${problems.length}%d ; acc ${right/problems.length}%2.2f")
        }



        def printOut() {
            val probdir = "data/problems/"
            val dir = new File(probdir)

            //Dont train on the dev or test sets
            val dontrain = Source.fromFile("data/dev.indexes").mkString.split("\n").toList ++ Source.fromFile("data/test.indexes").mkString.split("\n").toList
            val problems = dir.list filter (_.endsWith("mrs")) filter { x => !(dontrain contains x)}
            problems foreach { x => 

                //this reads the MRSes of each problem
		var fi = Source.fromFile(probdir+x)
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()

                // create a World from MRSes of problem statement sentences
                val w = parseStory(sentences)
                println(x.split("\\.")(0))
                var fi2 = Source.fromFile(probdir+x.split("\\.")(0)+".txt")
                println(fi2.mkString)
                fi2.close()
                w.EntityID foreach {case (k,v) => v.print()}

                //ILP
                var pidx = x.split("\\.")(0).toInt
                var eqs = getEqs(pidx) 
                if (verify(eqs,w)) eqs foreach {x => procEq(x,w)}
            }
        }

        def verify(eqs:ArrayBuffer[String],w:World): Boolean = {
            if (eqs.length == 0) return false 

            var e = eqs(0).split(" ") filter (_.charAt(0).isDigit) map (_.toFloat)
            //print(e.toList,w.numbers.map(_.card))
            if (w.numbers map (_.card) forall (x => e exists (g.close(x,_)))) return true
            else return false
        }

        class Trip(val e1:String,val e2:String,val op:String){
        }

        def procEq(e:String,w:World): ArrayBuffer[Trip] = {
            var spl = e.split(" ")
            var triples = new ArrayBuffer[Trip]
            var stack = new Stack[String]()
            spl foreach {x =>
                if ("/-+=*" contains x) {
                    var a = stack.pop
                    var b = stack.pop
                    var c = b+" "+a+" "+x
                    stack.push(c)
                    triples.append(new Trip(a,b,x))
                    if (!(w.Compounds contains c)) {
                        //print(a,b,c)
                        w.compoundEntity(c,lookup(a,w).get,lookup(b,w).get)
                    }

                }
                else stack.push(x)
                //println(stack)
            }
            return triples
        }

        def lookup(i:String,w:World): Option[w.Entity] = {
            if (i == "x") {
                var matches = w.variables() map (_._2)
                return matches.headOption
            } else if ("+-=/*" contains i.takeRight(1)) {
                return Some(w.Compounds(i))
            } else {
                var matches = w.EntityID map (_._2) filter (x => g.close(x.card,i.toFloat))
                return matches.headOption
            }
            return None
        }


        def train(ofi:String) {

            val probdir = "data/problems/"
            val dir = new File(probdir)

            //Dont train on the dev or test sets
            val dontrain = Source.fromFile("data/dev.indexes").mkString.split("\n").toList ++ Source.fromFile("data/test.indexes").mkString.split("\n").toList
            val problems = dir.list filter (_.endsWith("mrs")) filter { x => !(dontrain contains x)}

            //load answers and equations
            var answers = Source.fromFile("data/a.txt").mkString.split("\n") map (_.toFloat)
            var equations = Source.fromFile("data/eq.txt").mkString.split("\n")

            //produce training vectors for each problem
            var vecs = new ArrayBuffer[Array[Double]]
            val output = new PrintWriter(ofi)
            problems foreach { x => 

                //this reads the MRSes of each problem
		var fi = Source.fromFile(probdir+x)
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()

                // create a World from MRSes of problem statement sentences
                val w = parseStory(sentences)
                //w.numbers foreach {x=>x.print()}

                //this gets the associated answer and equation
                var pidx = x.split("\\.")(0).toInt
                var a = answers(pidx)
                //var e = equations(pidx)
                var eqs = getEqs(pidx)
                if (verify(eqs,w)) {
                    var trips = new ArrayBuffer[Trip]
                    eqs foreach {e => 
                        //println(e)
                        try {
                            trips ++= procEq(e,w) 
                        } catch {case _ : Throwable => {} } 
                    }
                    //println(trips.length)
                    trips.toSet
                    trips filter {_.op != "="} foreach {y =>
                        val e1 = lookup(y.e1,w)
                        val e2 = lookup(y.e2,w)
                        e1 match {
                            case Some(a) => {
                                e2 match {
                                    case Some(b) =>  {

                                        output.write(g.ops.indexOf(y.op)+" ")
                                        var j = 1
                                        var vec = w.handmadeVector(g.m,g.REL_LIST,a,b)
                                        vec foreach {z => output.write(j.toString+":"+z+" ");j+=1}
                                        output.write("\n")
                                    }
                                    case None => {} 
                                }
                            }
                            case None => {}
                        }
                    }
                }
            }
            output.close
        }

        def getEqs(i:Int,k:Int=100): ArrayBuffer[String] = {
            var fi = Source.fromFile(f"data/ILP.out/q$i%03d.txt.out").mkString.split("\n") filter (_ contains "EXPR: 1")
            var cEqs = new ArrayBuffer[String]
            fi foreach {x =>
                cEqs += x.split("\\|")(7).trim
            }
            return cEqs
        }

	def main(args:Array[String]) {
            val probdir = "data/problems/"
            val REL_LIST= Source.fromFile("data/rel_list.txt").mkString.split("\n").distinct
            //REL_LIST foreach {x => println(x)}
            val dir = new File(probdir)
            var files = new Array[String](0)
            args(0) match {
                case "printout" => {
                    printOut()
                }
                case "PCA" => {
                    doPCA()
                }
                case "rm" => {
                    removeProblems()
                }
                case "listRels" => {
                    listRels()
                }
		case "ILP" => {
		    ILPout()
		}
                case "split" => {
                    split()
                }
                case "train" => {
                    train(args(1))
                }
                case "test" => {
                    val test = Source.fromFile("data/test.indexes").mkString.split("\n").toList
                    files = dir.list filter (_.endsWith("mrs")) filter { x => (test contains x)}
                }
                case "dev" => {
			dev()
                }
                case _ => {
                    println("requrires test, train, or dev arg"); System.exit(1)
                }
            }

	}	
}

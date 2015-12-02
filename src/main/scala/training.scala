import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import java.io._
import libsvm.svm._
import breeze.linalg._

object gl {
        //def REL_LIST= Source.fromFile("data/rel_list.txt").mkString.split("\n").distinct
        def REL_LIST= Source.fromFile("data/relsReduced.txt").mkString.split("\n").distinct
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


object training {

        def verify(eqs:ArrayBuffer[String],w:World): Boolean = {
            if (eqs.length == 0) return false 

            var e = eqs(0).split(" ") filter (_.charAt(0).isDigit) map (_.toFloat)
            //print(e.toList,w.numbers.map(_.card))
            if (w.numbers map (_.card) forall (x => e exists (gl.close(x,_)))) return true
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
        val ofi = args(0)
        val probdir = "data/problems/"
        val dir = new File(probdir)

        //Dont train on the dev or test sets
        val dontrain = Source.fromFile("data/dev.indexes").mkString.split("\n").toList ++ Source.fromFile("data/test.indexes").mkString.split("\n").toList
        val problems = dir.list filter (_.endsWith("mrs")) filter { x => !(dontrain contains x)}

        //load answers and equations
        var answers = Source.fromFile("data/a.txt").mkString.split("\n") map (_.toFloat)

        //produce training vectors for each problem
        var vecs = new ArrayBuffer[Array[Double]]
        val output = new PrintWriter(ofi)
        var count = 0
        problems foreach { x => 

            //this reads the MRSes of each problem
            var fi = Source.fromFile(probdir+x)
            var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
            fi.close()

            var pidx = x.split("\\.")(0).toInt

            val w = parseStory(sentences)
            /*
            val oos = new ObjectInputStream(new FileInputStream(probdir+pidx+".world"))
            val wt = oos.readObject()
            val w = wt.asInstanceOf[World]
            w match {
                case x: World => x
                case _ => sys.error("Expected World, Fail")
            }
            */

            //this gets the associated answer and equation
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
                                    //This is dumb erase it after press
                                    if (count%5==0) {
                                        println("+++++++++")
                                        println(count/5)
                                        a.print()
                                        b.print()
                                    }
                                    count += 1
                                    



                                    output.write(g.ops.indexOf(y.op)+" ")
                                    var j = 1
                                    var vec = w.handmadeVector(gl.m,gl.REL_LIST,a,b)
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
        var fi = Source.fromFile(f"data/11.19.ILP.out/q$i%d.ilp.txt.out").mkString.split("\n") filter (_ contains "EXPR: 1")
        var cEqs = new ArrayBuffer[String]
        fi foreach {x =>
            cEqs += x.split("\\|")(7).trim
        }
        cEqs foreach println
        return cEqs
        
    }

}


import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import breeze.linalg._

class World() {
    var EntityNames = Map[String,String]()
    var EntityID = Map[String,Entity]()
    var EntityLBL = Map[String,String]()
    var IDtoID = Map[String,String]()
    var EntityList = new ListBuffer[(String,String)]
    var Compounds = Map[String,Entity]()
    var sidx = 0
    var entIdx = 0
    var myobjs = List[MRS]()

    val PL = List("they","them")
    val SINGF = List("she","her","hers","herself")
    val SINGM = List("he","him","his","himself")
    val FNAMES = List("Joan","Mary","Alyssa")
    val MNAMES = List("Sam","Tim","Tom")

    def splitID(id: String, delim: String): List[String] = {
        var spl = id split (delim)
        return List(spl(0),delim+spl(1))
    }

    class Reln(val r:String, val idx:Int, val o:Option[Entity] = None, val oidx:Any = None) {
        //if (Some(o)) 
        def print() {
            println(r,idx,o,oidx)
        }
    }

    class Entity(val cannonicalName:String) {
        var mentions = new ListBuffer[(String,MRS)]
        // relns are of the form Type, Arg, 
        var relations = new ListBuffer[Reln]
        var card: Float = 0
        var det = ""
        var n = ""
        var v = new ListBuffer[String]
        var adj = ""
        
        def carg(): String = {
            for (x<-mentions) if (x._2.carg != "None") return x._2.carg
            return "None"
        }

        def has_rel(rel:String): Boolean = {
            return relations.filter(x => x.r == rel).length > 0
        }

        def nvaap() {
            var mention = mentions(0)
            n = mention._2.word filter {x => !("_\".,?!" contains x) }//mention._2.rel.split("_n_")(0) filter {x => !("_\"" contains x)}
            val verbs = relations filter (x=> x.r contains "_v_")
            /*
            if (verbs.length > 0) {
                v = verbs map {y:Reln => y.r.split("_v_")(0) filter {x => !("_\"" contains x)}} flatten mkString " "
                    //foldLeft("":String)((x:String,y:String)=>x+" "+y)
            }
            */
            verbs foreach {x=>
                v += x.r.split("_v_")(0) filter {y=> !("_\"" contains y)}
            }
   
        }
                

        def print() {
            println(cannonicalName)
            println(n)
            println(v)

            println(card)
            for (x<-mentions) println(x._1)
            relations foreach {_.print()}
        }
    }

    def toFloat(x: String): Option[Float] = {
        try {
            Some(java.text.NumberFormat.getNumberInstance(java.util.Locale.US).parse(x.trim).floatValue)
        } catch {
            case e : Throwable => None
        }
    }


    def associate(id: String, ent: Entity) {
        val ids = for (m <- ent.mentions) yield m._2.idx
        val lbls = for (m <- ent.mentions) yield m._2.lbl
	var tospl = "x"
	if (id contains "i") tospl = "i"
        var spl = id split(tospl)
        var s = spl(0)
        var idx = tospl + spl(1)
        val sameLbl = myobjs filter (_.idx.startsWith(s)) filter (lbls contains _.lbl)
        for (e <- sameLbl) {
            if (e.rel contains "card_rel"){
                toFloat(e.carg) match {
                    case Some(i) => ent.card = i
                    case None => {println("CARD_REL CASE NON")}
                }
                //ent.card = e.carg
            }
        }

        toFloat(ent.carg) match {
            case Some(i) => ent.card = i
            case None => {}
        }

        val sEnts = myobjs filter (_.idx.startsWith(s)) filter (_.args.drop(1) contains idx)
        for (e <- sEnts) {
            var argidx = e.args.indexOf(idx)
            if (e.rel contains "card_rel"){
                toFloat(e.carg) match {
                    case Some(i) => ent.card = i
                    case None => {println("CARD_REL CASE NON")}
		}
            /*
                if (ent.card != None) {
                    if (ent.card != e.carg) {
                        print("CARG MISMATCH")
                    }
                } 
                else ent.card = e.carg
            */
            }
            else{
                ent.relations += new Reln(e.pRel,argidx)
                for (oidx <- e.args filter (_.startsWith("x"))) {
                    if (oidx != idx) {
                        var oargidx = e.args.indexOf(oidx)
                        ent.relations += new Reln(e.rel, argidx, Some(EntityID(IDtoID(s+oidx))),oargidx)
                    }
                }
            }
        }
    }



    def numbers = EntityID.toList sortWith (_._1 < _._1) map (_._2) filter (x=>((x.card != 0.0)))//||x.has_rel("much-many_a_rel")))

    def update(objs:List[MRS]) {
        this.myobjs = objs
        this.myobjs foreach {x => x.idx = if (x.args.length>0) sidx.toString + x.args(0) else "None" }
	//for (o <- objs filter (x=>(x.args(0).startsWith("x")||x.args(0).startsWith("i"))) filter (x => ! (x.rel contains "_q_"))) {
	for (o <- objs filter (x=>(x.args(0).startsWith("x"))) filter (x => ! (x.rel contains "_q_"))) {
            // this is an entity
            // we need to put in in the entities map
            // but we wanna make sure it's not coreferential w/ somethin in the map first
            var id = sidx.toString + o.args(0)
            var name = "none"
            if (EntityID contains id) {
                EntityID(id).mentions += ((o.word,o))
            //} 
            //else if (EntityLBL contains o.lbl) {
            //    EntityID(EntityLBL(o.lbl)).mentions += ((o.word,o))
            } else {
                // first coref?
                name = coref(o)
                var added = false
                if (( FNAMES contains name )|| ( MNAMES contains name)){
                    if (EntityNames contains name) {
                        EntityID(EntityNames(name)).mentions += ((o.word,o))
                        IDtoID += (id -> EntityNames(name))
                        EntityLBL += (o.lbl -> EntityNames(name))
                        added = true
                    } else {
                        EntityNames += (name -> id)
                    }
                }
                if (! added) {
                    // update entity entry{
                    name = o.word
                    EntityID += (id -> new Entity(id))
                    EntityLBL += (o.lbl -> id)
                    EntityID(id).mentions += ((name,o))
                    IDtoID += (id -> id)
                }
            }
            EntityList += ((name,id))
        }
        EntityID foreach {case (k,v) => associate(k,v);v.nvaap()}

        //deal w/ part_of_rel
        objs filter {x => x.rel contains "part_of_rel"} foreach { x=>
            EntityID(IDtoID(sidx.toString + x.args(1))).card = EntityID(IDtoID(sidx.toString + x.args(0))).card 
            EntityID(IDtoID(sidx.toString + x.args(0))).card = 0:Float
        }
        //EntityID foreach {case (k,v) => v.print()}

    }

    def variables(): Map[String,Entity] = {
        var vars = EntityID filter {case (k,v) => v.has_rel("much-many_a_rel")} 
        //vars foreach {case (k,v) => v.print()}
        return vars

    }

    def compoundEntity(x:String,e1:Entity,e2:Entity){
        if ("+*" contains x.takeRight(1)) {
            Compounds += ((x,e2))
        } else {
            Compounds += ((x,e1))
        }
    }
        


    def coref(o:MRS): String = {
        //println("coreffing")
        //print(o.word)
        if (SINGF.contains(o.word.toLowerCase()) || SINGM.contains(o.word.toLowerCase())){
            var poss = EntityID filter {case (k,v) => v.carg() != "None"} map (_._2) filter (_.carg.charAt(0).isLetter)
            if (poss.toList.length>0) return poss.last.carg
            else return o.word
        }
        /*
        if (SINGF.contains(o.word.toLowerCase())) {
            print(o.word)
            //return (EntityList filter {case (n,_) => FNAMES.contains(n)}).last._1
        } else if (SINGM.contains(o.word.toLowerCase())) {
            print(o.word)
            return (EntityList filter {case (n,_) => MNAMES.contains(n)}).last._1
        } else if (PL.contains(o.word.toLowerCase())) {
            // this is more complicated based on agency and etc... 
            return o.word
        }*/ 
        else { return o.word }
    }

    def sortByCannonMention(e1:Entity,e2:Entity) = {
        e1.cannonicalName < e2.cannonicalName
    }

    def vectorize(REL_LIST:Array[String],s:Int){
        //val numbers = EntityID map (_._2) filter (_.card != None) filter (_.card.asInstanceOf[String].charAt(0).isDigit)
        val sNumbers = this.numbers.toList.sortWith(sortByCannonMention)
        for (i <- 1 until sNumbers.length) {
            var j = 1
            print(s.toString + " ")
            vector(REL_LIST,sNumbers(i-1),sNumbers(i)) foreach {x => print(j.toString+":"+x.toString+" ");j+=1}
            println()
        }
    }

    def vectorize_node(REL_LIST:Array[String]): List[Array[libsvm.svm_node]] = {
        //val numbers = EntityID map (_._2) filter (_.card != None) filter (_.card.asInstanceOf[String].charAt(0).isDigit)
        val sNumbers = this.numbers.toList.sortWith(sortByCannonMention)
        var vecs = new ListBuffer[Array[libsvm.svm_node]]
        for (i <- 1 until sNumbers.length) {
             var tmp = vector(REL_LIST,sNumbers(i-1),sNumbers(i))
             var tmparray = new Array[libsvm.svm_node](tmp.length)
             var j = 0
             tmp.zipWithIndex foreach { x =>
                 var node = new libsvm.svm_node
                 node.index = x._2+1
                 node.value = x._1
                 tmparray(j) = node
                 j+=1
             }
             vecs += tmparray
        }
        return vecs.toList
    }


    def bVector(REL_LIST:Array[String],e1:Entity,e2:Entity): DenseVector[Double] = {
        val vec = DenseVector.zeros[Double](REL_LIST.length*2)
        e1.relations foreach {x => vec(REL_LIST.indexOf(x.r))=1}
        e2.relations foreach {x => vec(REL_LIST.length + REL_LIST.indexOf(x.r))=1}
        return vec
    }


    def handmadeVector(m:Word2Vec,REL_LIST:Array[String],e1:Entity,e2:Entity): Array[Double] = {
        var vec = new ArrayBuffer[Double]
        //vec += e1nvec
        //vec += e2nvec
        //println(e1.n, e2.n, m vector (e1.n))
        if ((m contains e1.n) && (m contains e2.n)) vec += m cosine (e1.n, e2.n); else vec+= -1
        if ((e1.v.length>0) && (e2.v.length>0)) {
            vec ++= m vector(e1.v(0)) map (_.toDouble)
            vec ++= m vector(e2.v(0)) map (_.toDouble)
            if ((m contains e1.v(0)) && (m contains e2.v(0))) vec += m cosine (e1.v(0), e2.v(0)); else vec+= -1
        } else {vec += -1; vec ++= Array[Double](400)}
        if ((e1.v.length>1) && (e2.v.length>1)) {
            //vec ++= m vector(e1.v(1)) map (_.toDouble)
            //vec ++= m vector(e2.v(1)) map (_.toDouble)
            if ((m contains e1.v(1)) && (m contains e2.v(1))) vec += m cosine (e1.v(1), e2.v(1)); else vec+= -1
        } else {vec += -1}//; vec ++= Array[Double](400)}
        REL_LIST foreach {x => if (e1.relations map (_.r) contains x) vec += 1.0 ; else vec += 0.0 }
        REL_LIST foreach {x => if (e2.relations map (_.r) contains x) vec += 1.0 ; else vec += 0.0 }
        return vec.toArray
    }

    def vector(REL_LIST:Array[String],e1:Entity,e2:Entity): List[Int] = {
        val vec = ListBuffer.fill(REL_LIST.length*2)(0)
        e1.relations foreach {x => vec(REL_LIST.indexOf(x.r))=1}
        e2.relations foreach {x => vec(REL_LIST.length + REL_LIST.indexOf(x.r))=1}
        return vec.toList
    }
}


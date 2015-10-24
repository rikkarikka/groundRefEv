import scala.io.Source
import java.io._

object splitQuestion {

def main(args: Array[String]) {
	
	var files = new File("problems").listFiles
	for (x <- files) {
		println(x)
		var fi = Source.fromFile(x)
		val f = new PrintWriter(x+".split")
		var fo = fi.mkString.trim.split("\\. ")
		fi.close()
		for(x<-fo){
			if (!x.trim.endsWith("?")) f.write(x.concat(".")) else f.write(x)
			f.write("\n")
		}
		f.close()
	}
}
}

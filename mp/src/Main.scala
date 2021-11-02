import es.tmoor.mineputer.parsing.Tokeniser
import es.tmoor.mineputer.parsing.MCParser
import es.tmoor.mineputer.parsing.MCParser.Name
import es.tmoor.mineputer.parsing.MCParser.Handler
import es.tmoor.mineputer.parsing.MCParser.CallFun
import es.tmoor.mineputer.Compiler

object Main extends App {
  val input = io.Source.fromFile("example/addOne.mp").mkString
  val tokenParseRes = Tokeniser.tokenSeq.parseAll(input)
  println(tokenParseRes.get)
  
  val parseRes = MCParser.statement.+.parseAll(tokenParseRes.get)
  println(parseRes)
  val compiler = new Compiler(parseRes.get)
  
}

/*

  val test1 = new Environment {
    var x = 0
    def init(): Unit = {}
    def handle(obj: RTVal): Unit = {
      println(s"Handling $obj")
      obj match {
        case RTObj(RTInt(0)::RTInt(1)::Nil) => x += 1
        case RTObj(RTInt(0)::RTInt(2)::Nil) => x -= 1
        case RTObj(RTInt(1)::RTInt(1)::RTInt(n)::Nil) => x += n
        case RTObj(RTInt(1)::RTInt(2)::RTInt(n)::Nil) => x -= n
        case RTObj(RTInt(2)::(rsp: RTMessageBox)::Nil) => rsp.reply(RTInt(x))
        case _ => 
      }
      println(s"X is now $x")
    }
  }
  val test2 = new Environment {
    def init(): Unit = {
      val incrementX = RTObj(RTInt(0)::RTInt(1)::Nil)
      send(incrementX, "Server")
      send(incrementX, "Server")
      val getX = RTInt(2)
      val result = sendWithResponse(getX, "Server")
      println(s"result is $result")
    }
    def handle(obj: RTVal): Unit = {}
  }
  println(test1.globals)
  println(test2.globals)
  test1.register("Server")
  test2.register()
}
*/
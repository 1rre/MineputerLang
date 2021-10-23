import es.tmoor.mineputer.parsing.Tokeniser
import es.tmoor.mineputer.parsing.MCParser
import es.tmoor.mineputer.parsing.MCParser.Host
import es.tmoor.mineputer.parsing.MCParser.Client
import es.tmoor.mineputer.parsing.MCParser.Name
import es.tmoor.mineputer.parsing.MCParser.Handler
import es.tmoor.mineputer.parsing.MCParser.MsgNoAwait
import es.tmoor.mineputer.environment._
import es.tmoor.mineputer.environment.Environment._

object Main extends App {

  /*
  val in = "'server'#('modify x', ('increment', b+2), ('return', 'true')) -> sender#'get atomic name'"
  import es.tmoor.mineputer.parsing.{Tokeniser, MCParser}
  val result = es.tmoor.mineputer.parsing.Tokeniser.tokenSeq.parseAll(in)
  println(result)
  result match {
    case Tokeniser.NoMatch(idx) => 
    case Tokeniser.Match(idx, r) =>
      println(MCParser.statement.*.parseAll(r))
  }
  */


  
  def testAsTokeniser(input: String) = {
    import es.tmoor.mineputer.parsing.Tokeniser._
    val result = tokenSeq.parseAll(input)
    result match {
      case NoMatch(idx) =>
        println(input.take(idx))
        Nil
      case Match(idx, result) =>
        testAsMCParser(result)
    }
  }
  def testAsMCParser(input: Seq[Tokeniser.Token]) = {
    import es.tmoor.mineputer.parsing.MCParser._
    val result = statement.+.parseAll(input)
    result match {
      case NoMatch(idx) =>
        println(input.take(idx))
        Nil
      case Match(idx, result) =>
        result
    }
  }
  def testServer() = {
    val input = io.Source.fromFile("example/host/server2.mp").mkString
    testAsTokeniser(input)
  }
  def testClient() = {
    val input = io.Source.fromFile("example/client/client2.mp").mkString
    testAsTokeniser(input)
  }

  val clientCode = testClient()
  val serverCode = testServer()
  val server = serverCode collectFirst {
    case Name(name) => new Host(serverCode, name)
  } getOrElse new Host(serverCode)
  val client = clientCode collectFirst {
    case Name(name) => new Host(clientCode, name)
  } getOrElse new Host(clientCode)
  val testMsgBox = new RTMessageBox()
  server.register()
  client.register()
  Thread.sleep(10)
  println(client.globals)
  println(server.globals)
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
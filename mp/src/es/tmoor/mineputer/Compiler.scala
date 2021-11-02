package es.tmoor.mineputer

import es.tmoor.mineputer.parsing.MCParser
import es.tmoor.mineputer.parsing.MCParser.Name
import es.tmoor.mineputer.parsing.MCParser.Handler
import es.tmoor.mineputer.parsing.MCParser.CallFun
import es.tmoor.mineputer.parsing.MCParser.Record

class Compiler(statements: Seq[MCParser.Statement]) {
  class ProcessedHandler
  val handlers = statements.collect {
    case h: Handler => h
  }
}
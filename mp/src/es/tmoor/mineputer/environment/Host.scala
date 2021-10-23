package es.tmoor.mineputer.environment

import es.tmoor.mineputer.parsing.MCParser.Statement
import Environment._
import es.tmoor.mineputer.parsing.MCParser
import collection.mutable.Map
import es.tmoor.mineputer.parsing.MCParser._
import scala.util.Random

class Host(val statements: Seq[Statement], name: String = Random.alphanumeric.take(32).mkString)
    extends Environment {
  statements.collect {
    case MCParser.Record(name, t, initial) =>
      globals += name -> ((t, resolveGlobal(initial)))
  }
  val handlers = statements.collect {
    case Handler(signature, cases) => signature -> cases
  }
  def register(): Unit = register(name)
  def init() = statements.collect {
    case MsgNoAwait(x, y) =>
      rtValue(x, Map()).asInstanceOf[RTSendable].send(rtValue(y, Map()))
  }
  def matchType(v: RTVal, t: Type): Boolean = {
    (v, t) match {
      case (RTInt(value), IntType) => true
      case (RTFloat(value), FpType) => true
      case (RTObj(members), ObjType(m)) =>
        members.size == m.size && members.zip(m).forall(mm => matchType(mm._1, mm._2))
      case (RTAtom(name), AtomType) => true
      case (_: RTMessageBox, NodeType) => true
      case _ => false
    }
  }
  def matchSig(s: MCParser.Assignable, v: RTVal): Boolean = s match {
    case IntLiteral(value) => v == RTInt(value)
    case FPLiteral(value) => v == RTFloat(value)
    case ObjLiteral(members) =>
      val x: Seq[RTVal] = members.map(rtValue(_, Map()))
      v == RTObj(x)
    case AtomicLiteral(name) => v == RTAtom(name)
    case TypedVar(id, t) => matchType(v, t)
    case AssignableObj(members) if v.isInstanceOf[RTObj] =>
      val vObj = v.asInstanceOf[RTObj]
      vObj.members.size == members.size && vObj.members.zip(members).forall {
        case (a, b) => matchSig(b, a)
      }
    case AssignableObj(_) => false
  }
  def fillVars(obj: RTVal, dest: Assignable): VarMap = {
    dest match {
      case IntLiteral(value) => Map()
      case FPLiteral(value) => Map()
      case ObjLiteral(members) => Map()
      case AtomicLiteral(name) => Map()
      case TypedVar(id, t) =>
        Map(id -> (t, obj))
      case AssignableObj(members) =>
        obj
          .asInstanceOf[RTObj]
          .members
          .zip(members)
          .map {
            case (a, b) => fillVars(a, b)
          }
          .reduce(_ ++ _)
    }
  }
  def getHandler(handlers: Seq[Handler], obj: RTVal): Option[Fun] = {
    if (handlers.isEmpty) None
    else {
      val cases = handlers.head._2
      val vars = fillVars(obj, handlers.head._1)
      cases.collectFirst {
        case c if rtValue(c._1, vars).neZero => (c._2, vars)
      }
    }
  }
  def declVal(v: Assignable, n: RTVal, vars: VarMap): Unit = {
    v match {
      // TODO case IntLiteral(value) =>
      // TODO case FPLiteral(value) =>
      // TODO case ObjLiteral(members) =>
      // TODO case AtomicLiteral(name) =>
      case TypedVar(id, t) => 
        vars += id -> (t, n)
      case AssignableObj(members) =>
        n.asInstanceOf[RTObj].members.zip(members).map {
          case (a, b) => declVal(b, a, vars)
        }
      case _ =>
    }
  }
  def assignVar(v: Assignable, n: RTVal, vars: VarMap): Unit = {
    v match {
      // TODO case IntLiteral(value) =>
      // TODO case FPLiteral(value) =>
      // TODO case ObjLiteral(members) =>
      // TODO case AtomicLiteral(name) =>
      case TypedVar(id, t) => {
        if (vars.contains(id)) {
          vars(id) = (t, n)
        } else if (globals.contains(id)) {
          globals(id) = (t, n)
        }
      }
      case AssignableObj(members) =>
        n.asInstanceOf[RTObj].members.zip(members).map {
          case (a, b) => assignVar(b, a, vars)
        }
      case _ =>
    }
  }
  def handle(obj: RTVal): Unit = {
    val vh = handlers.filter(h => matchSig(h._1, obj))
    val handler = getHandler(vh, obj)
    handler.foreach {
      case (a, b) =>
        a.foreach {
          case MsgNoAwait(x, y) =>
            rtValue(x, b).asInstanceOf[RTSendable].send(rtValue(y, b))
          case Declaration(variable, value) => declVal(variable, rtValue(value, b), b)
          case Assignment(variable, value) => assignVar(variable, rtValue(value, b), b)
        }
    }
  }
}

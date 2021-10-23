package es.tmoor.mineputer.environment
import collection.mutable
import es.tmoor.mineputer.parsing.MCParser._

object Environment {
  sealed trait RTVal {
    def neZero: Boolean = false
    def ==(oth: RTVal): RTVal
    def !=(oth: RTVal): RTVal = new RTInt(!((this == oth).neZero))
  }
  sealed trait RTNumber extends RTVal {
    def +(oth: RTNumber): RTNumber
    def *(oth: RTNumber): RTNumber
    def /(oth: RTNumber): RTNumber
    def -(oth: RTNumber): RTNumber
    def %(oth: RTNumber): RTNumber
    def >(oth: RTNumber): RTNumber
    def <(oth: RTNumber): RTNumber
    def >=(oth: RTNumber): RTNumber
    def =<(oth: RTNumber): RTNumber
  }
  sealed trait RTSendable extends RTVal {
    def send(msg: RTVal): Unit
  }
  case class RTInt(value: Int) extends RTNumber {
    def this(v: Boolean) = this(if (v) 1 else 0)
    override def neZero: Boolean = value != 0
    def +(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTInt(value + v)
      case RTFloat(v) => RTFloat(value + v)
    }
    def -(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTInt(value - v)
      case RTFloat(v) => RTFloat(value - v)
    }
    def *(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTInt(value * v)
      case RTFloat(v) => RTFloat(value * v)
    }
    def /(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTInt(value / v)
      case RTFloat(v) => RTFloat(value / v)
    }
    def %(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTInt(value % v)
      case RTFloat(v) => RTFloat(value % v)
    }
    def >(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => new RTInt(value > v)
      case RTFloat(v) => new RTInt(value > v)
    }
    def <(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => new RTInt(value < v)
      case RTFloat(v) => new RTInt(value < v)
    }
    def >=(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => new RTInt(value >= v)
      case RTFloat(v) => new RTInt(value >= v)
    }
    def =<(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => new RTInt(value <= v)
      case RTFloat(v) => new RTInt(value <= v)
    }
    def ==(oth: RTVal): RTVal = oth match {
      case RTInt(v) => new RTInt(value == v)
      case RTFloat(v) => new RTInt(value == v)
      case _ => new RTInt(false)
    }
  }
  case class RTFloat(value: Double) extends RTNumber {
    override def neZero: Boolean = value != 0
    def +(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTFloat(value + v)
      case RTFloat(v) => RTFloat(value + v)
    }
    def -(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTFloat(value - v)
      case RTFloat(v) => RTFloat(value - v)
    }
    def *(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTFloat(value * v)
      case RTFloat(v) => RTFloat(value * v)
    }
    def /(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTFloat(value / v)
      case RTFloat(v) => RTFloat(value / v)
    }
    def %(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => RTFloat(value % v)
      case RTFloat(v) => RTFloat(value % v)
    }
    def >(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => new RTInt(value > v)
      case RTFloat(v) => new RTInt(value > v)
    }
    def <(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => new RTInt(value < v)
      case RTFloat(v) => new RTInt(value < v)
    }
    def >=(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => new RTInt(value >= v)
      case RTFloat(v) => new RTInt(value >= v)
    }
    def =<(oth: RTNumber): RTNumber = oth match {
      case RTInt(v) => new RTInt(value <= v)
      case RTFloat(v) => new RTInt(value <= v)
    }
    def ==(oth: RTVal): RTVal = oth match {
      case RTInt(v) => new RTInt(value == v)
      case RTFloat(v) => new RTInt(value == v)
      case _ => RTInt(0)
    }
  }
  case class RTObj(members: Seq[RTVal]) extends RTVal {
    def ==(oth: RTVal): RTVal = oth match {
      case RTObj(m) => new RTInt(members.size == m.size && members.zip(m).forall {
        case (a,b) => (a == b).neZero
      })
      case _ => RTInt(0)
    }
  }
  case class RTAtom(name: String) extends RTVal with RTSendable {
    def send(msg: RTVal): Unit = Global.send(msg, name)
    def sendWithResponse(msg: RTVal): RTVal = Global.sendWithResponse(msg, name)
    def ==(oth: RTVal) =
      new RTInt(oth.isInstanceOf[RTAtom] && oth.asInstanceOf[RTAtom].name == name)
  }
  class RTMessageBox extends Global.MessageBox with RTVal with RTSendable {
    def send(msg: RTVal): Unit = reply(msg)
    def ==(oth: RTVal) = new RTInt(false)
  }
}
import Environment._
import Global._
trait Environment {
  type Handler = (Assignable, Seq[(Value, Seq[Operation])])
  type VarMap = collection.mutable.Map[String, (Type, RTVal)]
  type Fun = (Seq[Operation], VarMap)
  protected def rtMember(obj: Value, member: Value, locals: VarMap): RTVal =
    (rtValue(obj, locals), rtValue(member, locals)) match {
      case (RTObj(members), RTInt(i)) => members.applyOrElse(i, null)
      case _ => null
    }
  protected def rtValue(v: Value, locals: VarMap): RTVal = v match {
    case IDName("self") => RTAtom(Global.getName(this))
    case IntLiteral(value) => RTInt(value)
    case FPLiteral(value) => RTFloat(value)
    case ObjLiteral(members) => RTObj(members.map(rtValue(_, locals)))
    case AtomicLiteral(name) => RTAtom(name)
    case IDName(name) =>
      if (locals.contains(name)) locals(name)._2
      else if (globals.contains(name)) globals(name)._2
      else sys.error(s"$name not found")
    case ObjExpr(members) => RTObj(members.map(rtValue(_, locals)))
    case Plus(v) => rtValue(v, locals).asInstanceOf[RTNumber]
    case Minus(v) => RTInt(0) - rtValue(v, locals).asInstanceOf[RTNumber]
    case MemberAccess(obj, member) =>
      rtMember(obj, member, locals)
    case Mul(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] * rtValue(y, locals).asInstanceOf[RTNumber]
    case Div(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] / rtValue(y, locals).asInstanceOf[RTNumber]
    case Rem(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] % rtValue(y, locals).asInstanceOf[RTNumber]
    case Add(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] + rtValue(y, locals).asInstanceOf[RTNumber]
    case Sub(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] - rtValue(y, locals).asInstanceOf[RTNumber]
    case Gt(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] > rtValue(y, locals).asInstanceOf[RTNumber]
    case Lt(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] < rtValue(y, locals).asInstanceOf[RTNumber]
    case Ge(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] >= rtValue(y, locals).asInstanceOf[RTNumber]
    case Le(x, y) =>
      rtValue(x, locals).asInstanceOf[RTNumber] =< rtValue(y, locals).asInstanceOf[RTNumber]
    case Eq(x, y) =>
      rtValue(x, locals) == rtValue(y, locals)
    case Ne(x, y) =>
      rtValue(x, locals) != rtValue(y, locals)
    case MsgAwait(x, y) =>
      rtValue(x, locals).asInstanceOf[RTAtom].sendWithResponse(rtValue(y, locals))
  }
  protected def globalNumber(v: Value, mp: Int = 1): RTNumber = resolveGlobal(v) match {
    case RTInt(value) => RTInt(value * mp)
    case RTFloat(value) => RTFloat(value * mp)
    case _ => null
  }
  protected def globalMember(obj: Value, member: Value): RTVal =
    (resolveGlobal(obj), resolveGlobal(member)) match {
      case (RTObj(members), RTInt(i)) => members.applyOrElse(i, null)
      case _ => null
    }
  protected def resolveGlobal(v: Value): RTVal = v match {
    case IDName("self") => RTAtom(Global.getName(this))
    case IntLiteral(value) => RTInt(value)
    case FPLiteral(value) => RTFloat(value)
    case ObjLiteral(members) => RTObj(members.map(resolveGlobal))
    case AtomicLiteral(name) => RTAtom(name)
    case IDName(name) => globals.get(name).map(_._2).orNull
    case ObjExpr(members) => RTObj(members.map(resolveGlobal))
    case Plus(v) => globalNumber(v)
    case Minus(v) => globalNumber(v, -1)
    case MemberAccess(obj, member) => globalMember(obj, member)
    case Mul(x, y) => globalNumber(x) * globalNumber(y)
    case Div(x, y) => globalNumber(x) / globalNumber(y)
    case Rem(x, y) => globalNumber(x) % globalNumber(y)
    case Add(x, y) => globalNumber(x) + globalNumber(y)
    case Sub(x, y) => globalNumber(x) - globalNumber(y)
    case Gt(x, y) => globalNumber(x) > globalNumber(y)
    case Lt(x, y) => globalNumber(x) < globalNumber(y)
    case Ge(x, y) => globalNumber(x) >= globalNumber(y)
    case Le(x, y) => globalNumber(x) =< globalNumber(y)
    case Eq(x, y) => globalNumber(x) == globalNumber(y)
    case Ne(x, y) => globalNumber(x) != globalNumber(y)
    case MsgAwait(x, y) =>
      resolveGlobal(x).asInstanceOf[RTAtom].sendWithResponse(resolveGlobal(y))
  }
  val globals: VarMap = mutable.Map[String, (Type, RTVal)]()
  final private val threads = collection.mutable.Buffer[Thread]()
  def init(): Unit
  def handle(obj: RTVal): Unit
  final def stop(): Unit = threads.foreach(_.stop())
  def register(name: String = util.Random.alphanumeric.take(32).mkString): Unit = {
    Global.register(this, name)
  }
}

package es.tmoor.mineputer.parsing
import es.tmoor.parsing._
import Tokeniser._

object MCParser extends Parsers[Token] {
  sealed trait Statement

  case class Name(name: String) extends Statement {
    
  }
  private val name: Parser[Statement] = Word("name") /> acceptType[Atom] #> {
    case Atom(name) => Name(name)
  }
  sealed trait Value
  sealed trait Assignable
  sealed trait Literal extends Value with Assignable
  case class IntLiteral(value: Int) extends Literal {
    
  }
  case class FPLiteral(value: Double) extends Literal {
    
  }
  case class ObjLiteral(members: Seq[Literal]) extends Literal {
    
  }
  case class AtomicLiteral(name: String) extends Literal {
    
  }
  private val intLiteral: Parser[Literal] = new PFParser({
    case Word(content) => content.toIntOption.isDefined
  }) #> {
    case Word(content) => IntLiteral(content.toInt)
  }
  private val floatLiteral: Parser[Literal] = new PFParser({
    case Word(content) => content.toDoubleOption.isDefined
  }) #> {
    case Word(content) => FPLiteral(content.toDouble)
  }
  private val atomicLiteral: Parser[Literal] = acceptType[Atom] #> {
    case Atom(name) => AtomicLiteral(name)
  }
  private val objLiteral: Parser[Literal] =
    (Symbol("(") /> (literal.+(Symbol(","))) <\ Symbol(")")) #> (ObjLiteral(_))
  private val literal = intLiteral | floatLiteral | objLiteral | atomicLiteral
  private val identifier = new PFParser({
    case Word(content) => content.head.isLower
  }) #> {
    case Word(content) => content
  }
  sealed trait Type
  case object IntType extends Type {
    
  }
  case object FpType extends Type {
    
  }
  case object AtomType extends Type {
    
  }
  case object NodeType extends Type {
    
  }
  case object AnyType extends Type  {
    
  }
  case class ObjType(members: Seq[Type]) extends Type {
    members.mkString("struct {", "; ", "};")
  }
  private val atomType = Word("Atom") >> AtomType.asInstanceOf[Type]
  private val intType = Word("Int") >> IntType.asInstanceOf[Type]
  private val fpType = Word("Fp") >> FpType.asInstanceOf[Type]
  private val nodeType = Word("Node") >> NodeType.asInstanceOf[Type]
  private val rawType = atomType | intType | fpType | nodeType
  private val complexType =
    Symbol("(") /> allType.+(Symbol(",")) #> (ObjType(_).asInstanceOf[Type]) <\ Symbol(")")
  private val allType: Parser[Type] = rawType | complexType
  case class TypedVar(id: String, t: Type) extends Assignable {
    
  }
   val varName = identifier ~ (Symbol(":") /> allType).? #> {
    case (id, Some(t)) => TypedVar(id, t)
    case (id, None) => TypedVar(id, AnyType)
  }

  case class AssignableObj(members: Seq[Assignable]) extends Assignable {
    
  }
  private val assignableObj: Parser[Assignable] =
    (Symbol("(") /> assignable.+(Symbol(",")) <\ Symbol(")")) #> (AssignableObj(_))
  private val assignableRoot =
    literal #> (_.asInstanceOf[Assignable]) | varName #> (_.asInstanceOf[Assignable])
  private val assignable = assignableRoot | assignableObj

  sealed trait Operation extends Value

  def always: Value = IntLiteral(1)
  private val predicateCheck = Word("when") /> expr
  private val opSeq = (Symbol("{") /> operation.* <\ Symbol("}"))
  private val pCases =
    (predicateCheck ~ opSeq).+ ~
      ((Word("otherwise") >> always) ~ opSeq) #> {
        case (a, b) => a :+ b
      }
  private val pOneCase =
    ((Symbol("{") /> operation.* <\ Symbol("}")) #> (ops => Seq((IntLiteral(1), ops))))
  case class Handler(signature: Assignable, cases: Seq[(Value, Seq[Value])]) extends Statement {
    def casesStr = cases.map {
      case (a,b) => s"($a) {${b.mkString(" ", "; ", "; ")}}"
    }.mkString("if ", " else if ", "")
    
  }
  private val handler: Parser[Statement] = Word("sig") /> assignable ~ (pOneCase | pCases) #> {
    case (a,b) => Handler(a,b)
  }
  private val exprBrackets = Symbol("[") /> expr <\ Symbol("]")
  case class IDName(name: String) extends Value {
    
  }
  private val idName: Parser[Value] = identifier #> (IDName(_))
  case class ObjExpr(members: Seq[Value]) extends Value {
    
  }
  private val objExpr: Parser[Value] =
    (Symbol("(") /> expr.+(Symbol(",")) <\ Symbol(")")) #> (ObjExpr(_))
  private val expr$0: Parser[Value] =
    exprBrackets | objExpr | literal #> (_.asInstanceOf[Value]) | idName
  case class Plus(v: Value) extends Value {
    
  }
  case class Minus(v: Value) extends Value {
    
  }
  private val unaryPlus: Parser[Value] = Symbol("+") /> expr$1 #> (Plus(_))
  private val unaryMinus: Parser[Value] = Symbol("-") /> expr$1 #> (Minus(_))
  private val expr$1: Parser[Value] = unaryPlus | unaryMinus | expr$0

  case class MemberAccess(obj: Value, member: Value) extends Value {
    
  }
  private val memberAccessTail: Parser[Value => Value] =
    Symbol("$") /> expr$1 #> (b => (a: Value) => MemberAccess(a, b))
  private val expr$2Tail = memberAccessTail
  private val expr$2: Parser[Value] = expr$1 ~ (expr$2Tail.*) #> {
    case (a, b) => b.foldLeft(a)((acc, v) => v(acc))
  }
  case class Mul(x: Value, y: Value) extends Value {
    
  }
  case class Div(x: Value, y: Value) extends Value {
    
  }
  case class Rem(x: Value, y: Value) extends Value {
    
  }
  private val mulTail: Parser[Value => Value] =
    Symbol("*") /> expr$2 #> (b => (a: Value) => Mul(a, b))
  private val divTail: Parser[Value => Value] =
    Symbol("/") /> expr$2 #> (b => (a: Value) => Div(a, b))
  private val remTail: Parser[Value => Value] =
    Symbol("%") /> expr$2 #> (b => (a: Value) => Rem(a, b))

  private val expr$3Tail = mulTail | divTail | remTail
  private val expr$3: Parser[Value] = expr$2 ~ (expr$3Tail.*) #> {
    case (a, b) => b.foldLeft(a)((acc, v) => v(acc))
  }

  case class Add(x: Value, y: Value) extends Value {
    
  }
  case class Sub(x: Value, y: Value) extends Value {
    
  }
  private val addTail: Parser[Value => Value] =
    Symbol("+") /> expr$3 #> (b => (a: Value) => Add(a, b))
  private val subTail: Parser[Value => Value] =
    Symbol("-") /> expr$3 #> (b => (a: Value) => Sub(a, b))

  private val expr$4Tail = addTail | subTail
  private val expr$4: Parser[Value] = expr$3 ~ (expr$4Tail.*) #> {
    case (a, b) => b.foldLeft(a)((acc, v) => v(acc))
  }

  case class Gt(x: Value, y: Value) extends Value {
    
  }
  case class Lt(x: Value, y: Value) extends Value {
    
  }
  case class Ge(x: Value, y: Value) extends Value {
    
  }
  case class Le(x: Value, y: Value) extends Value {
    
  }
  case class Eq(x: Value, y: Value) extends Value {
    
  }
  case class Ne(x: Value, y: Value) extends Value {
    
  }
  private val gtTail: Parser[Value => Value] =
    Symbol(">") /> expr$4 #> (b => (a: Value) => Gt(a, b))
  private val ltTail: Parser[Value => Value] =
    Symbol("<") /> expr$4 #> (b => (a: Value) => Lt(a, b))
  private val geTail: Parser[Value => Value] =
    Symbol(">=") /> expr$4 #> (b => (a: Value) => Ge(a, b))
  private val leTail: Parser[Value => Value] =
    Symbol("=<") /> expr$4 #> (b => (a: Value) => Le(a, b))
  private val eqTail: Parser[Value => Value] =
    Symbol("=") /> expr$4 #> (b => (a: Value) => Eq(a, b))
  private val neTail: Parser[Value => Value] =
    Symbol("/=") /> expr$4 #> (b => (a: Value) => Ne(a, b))

  private val expr$5Tail = gtTail | ltTail | geTail | leTail | eqTail | neTail
  private val expr$5: Parser[Value] = expr$4 ~ (expr$5Tail.*) #> {
    case (a, b) => b.foldLeft(a)((acc, v) => v(acc))
  }

  case class CallFun(x: Value, y: Value) extends Value with Statement with Operation {
    
  }
  private val callFunTail: Parser[Value => Value] =
    Symbol("#") /> expr$5 #> (b => (a: Value) => CallFun(a, b))

  private val expr$6Tail = callFunTail
  private val expr$6: Parser[Value] = expr$5 ~ (expr$6Tail.*) #> {
    case (a, b) => b.foldLeft(a)((acc, v) => v(acc))
  }

  def expr = expr$6

  case class Declaration(variable: Assignable, value: Value) extends Operation{

  }
  private val declaration: Parser[Operation] = assignable ~ (Symbol("<=") /> expr) #> {
    case (a, b) => Declaration(a, b)
  }
  case class Assignment(variable: Assignable, value: Value) extends Operation {
  }
  private val assignment: Parser[Operation] = assignable ~ (Symbol("<-") /> expr) #> {
    case (a, b) => Assignment(a, b)
  }
  private val operation = assignment | declaration | expr
  case class Record(name: String, t: Type, initial: Value) extends Statement {
    
  }
  private val record =
    declaration #> {
      case Declaration(TypedVar(id, t), init) => Record(id, t, init).asInstanceOf[Statement]
    }

  def statement = name | record | handler
}

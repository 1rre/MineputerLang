package es.tmoor.parsing

import scala.reflect.ClassTag
import scala.util.Failure

trait Parsers[Elem] {
  sealed abstract trait ParseResult[T] {
    def get: T
  }
  case class NoMatch[T](idx: Int) extends ParseResult[T] {
    def get: T = null.asInstanceOf[T]
  }
  case class Match[T](idx: Int, result: T) extends ParseResult[T] {
    override def toString = s"Match: $result"
    def get: T = result
  }

  implicit class ElemImplicits(t: Elem) {
    def p: Parser[Elem] = new ElemParser(t)
    def ~[T1](other: => Parser[T1]): CombiParser[Elem,T1] = new CombiParser(new ElemParser(t), other)
    def ~(elem: Elem): CombiParser[Elem,Elem] = new CombiParser(new ElemParser(t), new ElemParser(elem))
    def |[T1 >: Elem](other: => Parser[T1]): ChoiceParser[Elem,T1] = new ChoiceParser(new ElemParser(t), other)
    def #>[T1](transform: PartialFunction[Elem,T1]): Parser[T1] = new TransformParser(new ElemParser(t), transform)
    def #>[T1](transform: Elem => T1): Parser[T1] = new TransformParser(new ElemParser(t), transform)
    def >>[T1](res: => T1): Parser[T1] = new ElemParser(t) #> (_ => res)
    def />[T1](next: => Parser[T1]): Parser[T1] = new ElemParser(t) ~ next #> (_._2)
    def <\(next: => Parser[_]): Parser[Elem] = new ElemParser(t) ~ next #> (_._1)
    def ? : Parser[Option[Elem]] = new OptionalParser(new ElemParser(t))
    def + : Parser[Seq[Elem]] = (
      (new ElemParser(t) ~ new ElemParser(t).+ #> {case (a,b) => a +: b})
        | (new ElemParser(t) #> (Seq(_)))
      )
    def +(sep: Parser[_]): Parser[Seq[Elem]] = (
      ((new ElemParser(t) <\ sep) ~ new ElemParser(t).+ #> {case (a,b) => a +: b})
        | (new ElemParser(t) #> (Seq(_)))
      )
    def * : Parser[Seq[Elem]] = new ElemParser(t).+.? #> (_.getOrElse(Nil))
    def *(sep: Parser[_]): Parser[Seq[Elem]] = new ElemParser(t).+(sep).? #> (_.getOrElse(Nil))
  }

  implicit class ElemSeqImplicits(s: Seq[Elem]) {
    def inOrder: Parser[Seq[Elem]] =
      s.map(new ElemParser(_) #> (Seq(_))).reduce(_~_ #> {
        case (a,b) => a++b
      })
    def choice: Parser[Elem] = s.map(new ElemParser(_)).reduce[Parser[Elem]](_|_)
  }
  implicit class ParserSeqImplicits[T](s: Seq[Parser[T]]) {
    def inOrder: Parser[Seq[T]] =
      s.reduce[Parser[_]]((acc,v) => (acc ~ v) #> {
        case (a, b) => a.asInstanceOf[Seq[T]] :+ b.asInstanceOf[T]
      }).asInstanceOf[Parser[Seq[T]]]
    def choice: Parser[T] = s.reduce[Parser[T]](_|_)
  }

  abstract class Parser[T] {
    def parseAll(input: Seq[Elem], from: Int = 0): ParseResult[T] = {
      parse(input, from) match {
        case Match(idx, result) if idx == input.length => Match(idx, result)
        case NoMatch(idx) => NoMatch(idx)
        case Match(idx, _) => NoMatch(idx)
      }
    }
    def parse(input: Seq[Elem], from: Int = 0): ParseResult[T]
    def ~[T1](other: => Parser[T1]): CombiParser[T,T1] = new CombiParser(this, other)
    def ~(elem: Elem): CombiParser[T,Elem] = new CombiParser(this, new ElemParser(elem))
    def |[T1 >: T](other: => Parser[T1]): ChoiceParser[T,T1] = new ChoiceParser(this, other)
    def #>[T1](transform: PartialFunction[T,T1]): Parser[T1] = new TransformParser(this, transform)
    def #>[T1](transform: T => T1): Parser[T1] = new TransformParser(this, transform)
    def >>[T1](res: => T1): Parser[T1] = this #> (_ => res)
    def />[T1](next: => Parser[T1]): Parser[T1] = this ~ next #> (_._2)
    def <\(next: => Parser[_]): Parser[T] = this ~ next #> (_._1)
    def />(elem: Elem): Parser[Elem] = this ~ elem #> (_._2)
    def <\(elem: Elem): Parser[T] = this ~ elem #> (_._1)
    def ? : Parser[Option[T]] = new OptionalParser(this)
    def + : Parser[Seq[T]] = (
      (this ~ this.+ #> (e => e._1 +: e._2))
        | (this #> (Seq(_)))
      )
    def +(sep: => Parser[_]): Parser[Seq[T]] = (this <\ sep).* ~ this #> (e => e._1 :+ e._2)
    def +(sep: Elem): Parser[Seq[T]] = (this <\ sep).* ~ this #> (e => e._1 :+ e._2)
    def * : Parser[Seq[T]] = this.+.? #> (_.getOrElse(Nil))
    def *(sep: => Parser[_]): Parser[Seq[T]] = this.+(sep).? #> (_.getOrElse(Nil))
    def *(sep: Elem): Parser[Seq[T]] = this.+(sep).? #> (_.getOrElse(Nil))
  }

  object Success extends Parser[Nothing] {
    def parse(input: Seq[Elem], from: Int): ParseResult[Nothing] = Match(from, null.asInstanceOf[Nothing])
  }
  object Failure extends Parser[Nothing] {
    def parse(input: Seq[Elem], from: Int): ParseResult[Nothing] = NoMatch(from)
  }
  class ElemParser(c: Elem) extends Parser[Elem] {
    def parse(input: Seq[Elem], from: Int): ParseResult[Elem] = {
      if (input.length <= from) NoMatch(from)
      else if (input(from) == c) Match(from + 1, c)
      else NoMatch(from)
    }
  }

  class OptionalParser[T](parseIn: Parser[T]) extends Parser[Option[T]] {
    def parse(input: Seq[Elem], from: Int): ParseResult[Option[T]] =
      parseIn.parse(input, from) match {
        case NoMatch(_) => Match(from, None)
        case Match(idx, result) => Match(idx, Some(result))
      }
  }

  class TransformParser[T1,T2](parseIn: Parser[T1], transform: PartialFunction[T1,T2]) extends Parser[T2] {
    def this(parseIn: Parser[T1], transform: T1 => T2) = this(parseIn, new PartialFunction[T1,T2] {
      def isDefinedAt(x: T1) = true
      def apply(x: T1) = transform(x)
    })
    def parse(input: Seq[Elem], from: Int): ParseResult[T2] = {
      parseIn.parse(input, from) match {
        case NoMatch(idx) => NoMatch(idx)
        case Match(idx, result) => Match(idx, transform(result))
      }
    }
  }
  class PFParser(pf: Elem => Boolean) extends Parser[Elem] {
    def this(pf: PartialFunction[Elem, Boolean]) = this(ch => pf.applyOrElse(ch, (_: Any) => false))
    def parse(input: Seq[Elem], from: Int): ParseResult[Elem] = {
      if (input.length <= from) NoMatch(from)
      else if (pf(input(from))) Match(from + 1, input(from))
      else NoMatch(from)
    }
  }
  def acceptType[T: ClassTag]: Parser[T] = new PFParser ({
    case x: T => true
  }).asInstanceOf[Parser[T]]
  class ChoiceParser[T1,T2 >: T1](left: => Parser[T1], right: => Parser[T2]) extends Parser[T2] {
    def parse(input: Seq[Elem], from: Int): ParseResult[T2] = {
      left.parse(input, from) match {
        case NoMatch(_) => right.parse(input, from)
        case result => result.asInstanceOf[ParseResult[T2]]
      }
    }
  }
  class CombiParser[T1,T2](left: => Parser[T1], right: => Parser[T2]) extends Parser[(T1,T2)] {
    def parse(input: Seq[Elem], from: Int): ParseResult[(T1,T2)] = {
      left.parse(input, from) match {
        case Match(i1, r1) =>
          right.parse(input, i1) match {
            case Match(i2, r2) => Match(i2, (r1,r2))
            case NoMatch(idx) => NoMatch(idx)
          }
        case NoMatch(idx) => NoMatch(idx)
      }
    }
  }
}
trait TextParser extends Parsers[Char] {
  def str(s: String): Parser[String] = s.toSeq.inOrder #> (_.mkString)
}
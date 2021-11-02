package es.tmoor.mineputer.parsing

import es.tmoor.parsing.TextParser

object Tokeniser extends TextParser {
  trait Token
  object Token {
    def ws: Token = Whitespace
    def atom(s: String): Token = Atom(s)
    def atom(s: Seq[Char]): Token = Atom(s.mkString)
    def atom(c: Char): Token = Atom(c.toString)
    def sym(s: String): Token = Symbol(s)
    def sym(s: Seq[Char]): Token = Symbol(s.mkString)
    def sym(c: Char): Token = Symbol(c.toString)
    def word(s: String): Token = Word(s)
    def word(s: Seq[Char]): Token = Word(s.mkString)
    def word(c: Char): Token = Word(c.toString)
  }
  object Whitespace extends Token
  private def lineComment = str("!") ~ new PFParser(_ != '\n').* >> Token.ws
  private def cantEndComment = new PFParser(_ != '-') >> Token.ws
  private def notEndComment =
    ('-' ~ new PFParser(_ != '!')) >> Token.ws | cantEndComment
  private val blockComment = str("!-") ~ notEndComment.* ~ str("-!") >> Token.ws
  private val comment = lineComment | blockComment
  private val anyBlank = new PFParser(_.isWhitespace) >> Token.ws
  private val whitespace = comment | anyBlank
  case class Atom(name: String) extends Token
  private val atom =
    '\'' /> (new PFParser(_ != '\'').+ #> (Token.atom(_))) <\ '\''
  case class Symbol(name: String) extends Token
  private val senders = str("#") #> (Token.sym(_))
  private val comparators =
    Seq(">=", "=<", "<=", "/=", "=", ">", "<").map(str).choice #> (Token.sym(_))
  private val assignment = str("<-") >> Token.sym("<-")
  private val brackets =
    Seq('(', ')', '{', '}', '[', ']').choice #> (Token.sym(_))
  private val operators =
    Seq('+', '-', '*', '/', '%', '~', '$').choice #> (Token.sym(_))
  private val punctuation = Seq(',', ':').choice #> (Token.sym(_))
  private val symbol: Parser[Token] =
    assignment | senders | comparators | brackets | operators | punctuation
  case class Word(content: String) extends Token
  private def identifierChars =
    ('a' to 'z') :++ ('A' to 'Z') :++ ('0' to '9') :+ '_' :+ '.'
  private val word: Parser[Token] = identifierChars.choice.+ #> (Token.word(_))
  val anyToken = atom | symbol | word | whitespace
  val tokenSeq = anyToken.* #> (_.filterNot(_ == Whitespace))
}

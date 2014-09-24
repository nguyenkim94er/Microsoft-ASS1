// Project name MP
// Create by: nhphung
// Create date: Aug 27, 2012
// Language: Scala
// Description: This file is about recognizer for MP language, see MP language specification for more information
import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh

trait BKOOLTokens extends StdTokens {
  // Adapted from StdTokens
  case class FloatLit(chars: String) extends Token {
    override def toString = "FloatLit: "+chars
  }
  case class IntLit(chars: String) extends Token {
    override def toString = "IntLit: "+chars
  }
  case class BooleanLit(chars: String) extends Token {
    override def toString = "BooleanLit: " + chars
  }
}

// TODO Copy your lexer here
class BKOOLLexer extends StdLexical with BKOOLTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh


  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      (r findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) =>
          Success(source.subSequence(offset, offset + matched.end).toString,
            in.drop(matched.end))
        case None =>
          Failure("string matching regex `" + r + "' expected but `" + in.first + "' found", in.drop(0))
      }
    }
  }

  // TODO student add code here to complete token
  reserved ++= List("bool", "extends", "string", "false", "break", "float", "then", "void",
  					"class", "for", "to", "null", "continue", "if", "until", "self",
  					"do", "integer", "while", "final", "downto", "new", "return", "else", "repeat", "true")

  delimiters ++= List(":", ";", "{", "}")

  override def token: Parser[Token] = {
    // Adapted from StdLexical
    (
	regex("[0-9]+".r)			^^ { IntLit(_) }
	|regex("[a-z]+".r)  		^^ {processIdent(_)}
	|EofCh 						^^^ EOF
    |delim

	)
  }

  override def whitespace: Parser[Any] = rep(
    whitespaceChar
      | '(' ~ '*' ~ comment
      | '(' ~ '*' ~> failure("unclosed comment"))

  override protected def comment: Parser[Any] = (
    '*' ~ ')' ^^ { case _ => ' ' }
    | chrExcept(EofCh) ~ comment)

}

import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.syntactical.StdTokenParsers 

class BKOOLRecognizer extends StdTokenParsers {
  type Tokens = BKOOLTokens
  val lexical = new BKOOLLexer
  def getLexical: BKOOLLexer = lexical

  def show(result: ParseResult[Any]): String = {
    result match {
      case Failure(msg, next) =>
        "Error at line "+ next.pos.line +" col "+ next.pos.column
	  case Error(msg,next) => 
		"Fatal error at line "+ next.pos.line +" col "+ next.pos.column
      case _ => "Successful"
    }
  }  
  
  def parse(s:String) = phrase(program)(new lexical.Scanner(s))
 // For BKOOLRecognizer, students may modify the following.
  def program: Parser[Any] =  rep(classdecl) 
  
  def vardecl: Parser[Any] = ident ~ ":" ~ "int" ~ ";"
  
  def classdecl: Parser[Any] = "class" ~ ident ~ opt("extends" ~ ident) ~ "{" ~ vardecl ~ "}" 
     
  def boollit: Parser[Any] = elem("boolean", _.isInstanceOf[lexical.BooleanLit])  
  
  def floatlit: Parser[Any] = elem("real", _.isInstanceOf[lexical.FloatLit]) 

  def intlit: Parser[Any] = elem("integer", _.isInstanceOf[lexical.IntLit]) 
 }
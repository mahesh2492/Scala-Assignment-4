/* Requirements
Implement the methods given in ArithmeticParser trait of template having ???(yet to be implemented) definition.
All tests cases, written in template should pass. For running test cases run command: sbt clean test */


package edu.knoldus.parsing

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

trait ArithmeticParser extends RegexParsers with Syntax {
 // Defined regex to parse integer
  def integer:Parser[IntegerLiteral] = """-?\d+""".r ^^ {s => IntegerLiteral(s.toInt)}

  def sum:Parser[Sum] = operand ~ "+" ~ operand^^{
  case(x ~ "+" ~y) => Sum(x,y)
  }

  def minus:Parser[Minus] = operand ~ "-" ~ operand^^{
  case(x~"-"~y) => Minus(x,y) }

  def product:Parser[Product] = operand ~ "*" ~ operand^^{
  case(x~"*"~y) => Product(x,y) }

  def divide:Parser[Divide] = operand ~ "/" ~ operand^^{
  case(x~"/"~y) => Divide(x,y) }

  def parenthesizedExpr = "("~expression~")" ^^ {case("("~i~")")=>i} 

  def operand = (integer | parenthesizedExpr)

  def expression:Parser[Expression] = ( sum | minus | product | divide | integer | parenthesizedExpr )

}

object ArithmeticParser extends ArithmeticParser {

  def parseExpr(s: CharSequence): Expression = {
    parseExpr(new CharSequenceReader(s))
  }

  def parseExpr(input: CharSequenceReader): ArithmeticParser.Expression = {
    parsePhrase(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, next) => throw new IllegalArgumentException(
        "Could not parse '" + input + "' near '" + next.pos.longString + ": " + msg)
    }
  }

  def parsePhrase(input: CharSequenceReader): ParseResult[Expression] = {
    phrase(expression)(input)
  }
}

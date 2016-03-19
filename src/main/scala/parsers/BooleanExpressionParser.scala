package parsers

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by Paul Lysak on 19.03.16.
  */
object BooleanExpressionParser extends JavaTokenParsers {
  import Ast._

  def parse(str: String): Ast.Expression =
    parseAll(expression, str) match {
      case Success(result, _) => result
      case failedOrIncomplete => throw new RuntimeException(failedOrIncomplete.toString)
    }

  private def expression: Parser[Expression] = combinationExpression | leftExpression

  private def combinationExpression: Parser[Expression] =  comment.? ~> or | and <~ comment.?
  /**
    * Expressions that can be used as left part of recursive expression
    * @return
    */
  private def leftExpression: Parser[Expression] = comment.? ~> not | brackets | variable <~ comment.?

  private def brackets: Parser[Expression] = "(" ~> expression <~ ")"

  private def variable: Parser[Variable] = ident  ^^ {case v => Variable(v.toLowerCase)}

  private def not: Parser[Not] = "not" ~> expression ^^ (Not(_))

  private def and: Parser[And] = leftExpression ~ ("and" ~> leftExpression).+ ^^
    {case (left: Expression) ~ (right: Seq[Expression]) => And(left +: right)}

  private def or: Parser[Or] = (and | leftExpression) ~ ("or" ~> (and | leftExpression)).+ ^^ {case left ~ right => Or(left +: right)}

  private def comment: Parser[String] = """/\*([^*]|\*[^/])*\*/""".r ^^^ ""
}

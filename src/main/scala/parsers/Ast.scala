package parsers

/**
  * Created by Paul Lysak on 19.03.16.
  */
object Ast {

  sealed trait Expression {
    def evaluate(input: Map[String, Boolean]): Boolean
  }

  case class And(expressions: Seq[Expression]) extends Expression {
    override def evaluate(input: Map[String, Boolean]) = expressions.map(_.evaluate(input)).forall(_ == true)
  }

  case class Or(expressions: Seq[Expression]) extends Expression {
    override def evaluate(input: Map[String, Boolean]) = expressions.map(_.evaluate(input)).exists(_ == true)
  }

  case class Not(expression: Expression) extends Expression {
    override def evaluate(input: Map[String, Boolean]) = !expression.evaluate(input)
  }

  case class Variable(name: String) extends Expression {
    override def evaluate(input: Map[String, Boolean]) = input(name)
  }
}

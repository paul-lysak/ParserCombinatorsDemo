package parsers

import org.scalatest.{ MustMatchers, WordSpec }

/**
  * Created by Paul Lysak on 19.03.16.
  */
class ParserSpec extends WordSpec with MustMatchers{
  import Ast._

  "Parser" must {
    "parse variable" in {
      Parser.parse("x") mustBe(Variable("x"))
    }

    "parse variable and convert case" in {
      Parser.parse("X") mustBe(Variable("x"))
    }


    "parse not" in {
      Parser.parse("not x") mustBe(Not(Variable("x")))
    }

    "parse and" in {
      Parser.parse("x and y") mustBe(And(Seq(Variable("x"), Variable("y"))))
    }

    "parse and 3" in {
      Parser.parse("x and y and z") mustBe(And(Seq(Variable("x"), Variable("y"), Variable("z"))))
    }

    "parse and with brackets" in {
      Parser.parse("(x and y)") mustBe(And(Seq(Variable("x"), Variable("y"))))
    }

    "parse or" in {
      Parser.parse("x or y") mustBe(Or(Seq(Variable("x"), Variable("y"))))
    }

    "parse or 3" in {
      Parser.parse("x or y or z") mustBe(Or(Seq(Variable("x"), Variable("y"), Variable("z"))))
    }


    "parse and/or with correct priority 1" in {
      Parser.parse("x and y or z") mustBe(Or(Seq(And(Seq(Variable("x"), Variable("y"))), Variable("z"))))
    }

    "parse and/or with correct priority 2" in {
      Parser.parse("x or y and z") mustBe(Or(Seq(Variable("x"), And(Seq(Variable("y"), Variable("z"))))))
    }

    "parse and/or with correct priority 3" in {
      Parser.parse("(x or y) and z") mustBe(And(Seq(Or(Seq(Variable("x"), Variable("y"))), Variable("z"))))
    }

    "parse and with comment" in {
      Parser.parse("x /* first var */ and y /* second var */") mustBe(And(Seq(Variable("x"), Variable("y"))))
    }
  }
}

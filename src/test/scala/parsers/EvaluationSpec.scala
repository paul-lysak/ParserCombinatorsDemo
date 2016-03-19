package parsers

import org.scalatest.{ MustMatchers, WordSpec }

/**
  * Created by Paul Lysak on 19.03.16.
  */
class EvaluationSpec extends WordSpec with MustMatchers{
  "Expression" must {
    "evaluate variable" in {
      testByTable("x", Seq(
        Map("x" -> false) -> false,
        Map("x" -> true) -> true
      ))
    }

    "evaluate not" in {
      testByTable("not x", Seq(
        Map("x" -> false) -> true,
        Map("x" -> true) -> false
      ))
    }


    "evaluate and" in {
      testByTable("x and y", Seq(
        Map("x" -> false, "y" -> false) -> false,
        Map("x" -> false, "y" -> true) -> false,
        Map("x" -> true, "y" -> false) -> false,
        Map("x" -> true, "y" -> true) -> true
      ))
    }

    "evaluate or" in {
      testByTable("x or y", Seq(
        Map("x" -> false, "y" -> false) -> false,
        Map("x" -> false, "y" -> true) -> true,
        Map("x" -> true, "y" -> false) -> true,
        Map("x" -> true, "y" -> true) -> true
      ))
    }

    "evaluate x and y or z" in {
      testByTable("x and y or z", Seq(
        Map("x" -> false, "y" -> false, "z" -> false) -> false,
        Map("x" -> false, "y" -> false, "z" -> true) -> true,
        Map("x" -> false, "y" -> true, "z" -> false) -> false,
        Map("x" -> false, "y" -> true, "z" -> true) -> true,
        Map("x" -> true, "y" -> false, "z" -> false) -> false,
        Map("x" -> true, "y" -> false, "z" -> true) -> true,
        Map("x" -> true, "y" -> true, "z" -> false) -> true,
        Map("x" -> true, "y" -> true, "z" -> true) -> true
      ))
    }
  }

  def testByTable(exprStr: String, table: Seq[(Map[String, Boolean], Boolean)]): Unit = {
    val expr = Parser.parse(exprStr)
    val out = for((in, expectedOut) <- table) yield {
      val actualOut = expr.evaluate(in)
      (actualOut, expectedOut)
    }
    val (actualOut, expectedOut) = out.unzip
    actualOut mustBe(expectedOut)
  }
}

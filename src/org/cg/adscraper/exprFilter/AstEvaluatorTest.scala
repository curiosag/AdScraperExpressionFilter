package org.cg.adscraper.exprFilter

import scala.util.parsing.combinator.Parsers
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.cg.scala.expressionparser._

class AstEvaluatorSuite extends JUnitSuite {

  def echo(s: String) =
    {
      System.out.println(s)
      s
    }

  @Before def initialize() {
  }

  def probeResult[T](comp: EvalResult[T], num: EvalResult[T]): Unit = probeResult(comp, num, identity)

  def probeResult[T](comp: EvalResult[T], num: EvalResult[T], feedback: (String) => String) =
    {
      (num, comp) match {
        case (EvalOk(x), EvalOk(y)) => assert(x == y)
        case _ => {
          fail(feedback("%s != %s".format(num.toString(), comp.toString())))
        }
      }
    }

  def expectException(f: () => Unit) =
    {
      try {
        f();
        fail("exception expected")
      } catch {
        case _: Throwable =>
      }
    }

  def parseAndPrint(conv: (AstNode) => String, s: String) = {
    val p = new ExprParser(AstEvaluator)
    val v = p.parse(s) match 
    {
      case ExprOk(node) => conv(node)
      case ExprErr(msg) => msg
    }
    echo(v)
  } 

  @Test def testParsing() {
    val expr = "1 < prize < 100 & (false | true) & (1 < 0 | 1 > 0) | ! prize > prize & scorn(me, you, us)"
  //   parseAndPrint((x) => AstEvaluator.print(x), expr)
 //   parseAndPrint((x) => TokenLabeller(x).get(), expr)
       parseAndPrint((x) => Ast2Dot(x).get(), expr)
  }
 
}

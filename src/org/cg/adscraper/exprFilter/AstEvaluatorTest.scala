package org.cg.adscraper.exprFilter

import org.scalatest.junit.JUnitSuite
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import scala.util.parsing.combinator

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
      case EvalOk(node) => conv(node)
      case EvalFail(msg) => msg
    }
    echo(v)
  } 

  @Test def testParsing() {
    val expr = "1 < prize < 100 & (false | true) & (1 < 0 | 1 > 0) | ! prize > prize & scorn(me, you, us)"
 //   parseAndPrint((x) => AstEvaluator.print(x), expr)
    parseAndPrint((x) => AstLabeller(x).get(), expr)
  }
 
}

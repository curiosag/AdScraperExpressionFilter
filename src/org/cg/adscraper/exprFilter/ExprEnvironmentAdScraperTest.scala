package org.cg.adscraper.exprFilter

import org.scalatest.junit.JUnitSuite
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import org.cg.ads.advalues._;
import org.cg.ads.filterlist._;
import java.util.ArrayList

import scala.util.parsing.combinator
import org.cg.scala.expressionparser._

class ExprEnvironmentAdScraperSuite extends JUnitSuite {

  val values = new ScrapedValues()
  val filters = new FilterList()
  val parser = new ExprParserAdScraper(values, filters)
  val env = new AdScraperEnvironment(values, filters)
  val c = new ExprEvaluatorAdScraper(values, filters)
  val ok = EvalOk("expected exception:");
  val evalTrue = EvalOk(true);
  val evalFalse = EvalOk(false);
  val unknownValueName = "unknown"

  values.add(ScrapedValue.create(ValueKind.prize, "1.1"))
  values.add(ScrapedValue.create(ValueKind.size, "x"))
  values.add(ScrapedValue.create(ValueKind.phone, "phone"))
  values.add(ScrapedValue.create(ValueKind.description, "bb aa bb cc dd"))

  val filter = new ArrayList[String]();
  filter.add("aa")
  filter.add("a")
  filter.add("xx")
  filters.add("filter1", filter)

  def echo(s: String) =
    {
      System.out.println(s)
      s
    }

  @Before def initialize() {
  }

  def probeResult[T](comp: EvalResult[T], num: EvalResult[T]): Unit = { probeResult(comp, num, identity) }

  def probeResult[T](comp: EvalResult[T], num: EvalResult[T], feedback: (String) => String) =
    {
      (num, comp) match {
        case (EvalOk(x), EvalOk(y)) => assert(x == y)
        case _ => {
          fail(feedback("%s != %s".format(num.toString(), comp.toString())))
        }
      }
    }

  def probeParseResult[T](r: ExprParseResult[EvalResult[T]], comp: EvalResult[T]): Unit = { probeParseResult(r, comp, identity) }

  def probeParseResult[T](r: ExprParseResult[EvalResult[T]], comp: EvalResult[T], feedback: (String) => String) =
    {
      r match {
        case ExprOk(x) => probeResult(x, comp)
        case ExprErr(x) => probeResult(EvalFail(x), comp)
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

  @Test def testNumberConversion() {
    probeResult(env.decodeNumber("0"), EvalOk(0));
    probeResult(env.decodeNumber("0.1"), EvalOk(0.1));
    probeResult(env.decodeNumber("0,1"), EvalOk(0.1));
    probeResult(env.decodeNumber("+0.1"), EvalOk(0.1));
    probeResult(env.decodeNumber("-0.1"), EvalOk(-0.1));
    expectException(() => probeResult(env.decodeNumber("1"), EvalOk(0)));
    expectException(() => probeResult(env.decodeNumber("x"), EvalOk(0)));
    expectException(() => probeResult(env.decodeNumber("1"), EvalFail("failure")));
  }

  @Test def testCtxGetValues() {

    probeResult(env.getCtxString("phone"), EvalOk("phone"));
    expectException(() => probeResult(env.getCtxString("unknown"), ok, echo));
    probeResult(env.getCtxString("prize"), EvalOk("1.1"));
    probeResult(env.getFilter("filter1"), EvalOk(filter))
    expectException(() => probeResult(env.getFilter("a non existing filter"), ok, echo));
  }

  @Test def testPassFilter() {
    probeResult(env.evalPassFilter(Id("description"), Id("filter1")), evalFalse)
    probeResult(env.evalPassFilter(Id("phone"), Id("filter1")), evalTrue)
    expectException(() => probeResult(env.evalPassFilter(Id(unknownValueName), Id("filter1")), ok, echo))
    expectException(() => probeResult(env.evalPassFilter(Id("phone"), Id(unknownValueName)), ok, echo))
  }

  @Test def testBinOps() {

    val bigger = Num("10")
    val smaller = Num("0")
    val same = Num("1.1")
    val invalidNumber = Num("x.x")
    val invalidOp = Op("?")

    probeResult(c.evalRelOp(Id("prize"), Op(">"), smaller), evalTrue);
    probeResult(c.evalRelOp(Id("prize"), Op("<"), bigger), evalTrue);
    probeResult(c.evalRelOp(Id("prize"), Op(">="), smaller), evalTrue);
    probeResult(c.evalRelOp(Id("prize"), Op("<="), bigger), evalTrue);
    probeResult(c.evalRelOp(Id("prize"), Op("=="), same), evalTrue);

    probeResult(c.evalRelOp(Id("prize"), Op(">"), bigger), evalFalse);
    probeResult(c.evalRelOp(Id("prize"), Op("<"), smaller), evalFalse);
    probeResult(c.evalRelOp(Id("prize"), Op(">="), bigger), evalFalse);
    probeResult(c.evalRelOp(Id("prize"), Op("<="), smaller), evalFalse);

    expectException(() => probeResult(c.evalRelOp(Id(unknownValueName), Op(">"), bigger), ok, echo));
    expectException(() => probeResult(c.evalRelOp(Id("prize"), Op("=="), invalidNumber), ok, echo));
    expectException(() => probeResult(c.evalRelOp(Id("prize"), invalidOp, same), ok, echo));
    expectException(() => probeResult(c.evalRelOp(Id(unknownValueName), invalidOp, invalidNumber), ok, echo));

    probeResult(c.evalBinOpBoolean(evalTrue, Op("&"), evalTrue), evalTrue);
    probeResult(c.evalBinOpBoolean(evalFalse, Op("&"), evalTrue), evalFalse);
    probeResult(c.evalBinOpBoolean(evalFalse, Op("|"), evalTrue), evalTrue);
    probeResult(c.evalBinOpBoolean(evalFalse, Op("|"), evalFalse), evalFalse);

    expectException(() => probeResult(c.evalBinOpBoolean(evalTrue, invalidOp, evalTrue), ok, echo));

    probeResult(c.evalBooleanNot(evalFalse, Op("!")), evalTrue);

  }

  @Test def testParsing() {
    val p = parser

    assertEquals(1, p.eval("1 < 2").status)
    assertEquals(0, p.eval("1 > 2").status)
    assertEquals(-1, p.eval("1 ? 2").status)

    probeParseResult(p.parse("1 < 2"), evalTrue)

    probeParseResult(p.parse("1 > 2"), evalFalse)
    probeParseResult(p.parse("! prize > 2"), evalTrue)
    probeParseResult(p.parse("1 < prize < 100 "), evalTrue)
    probeParseResult(p.parse("1 < prize < 100 & (false | true) & (1 < 0 | 1 > 0) & ! prize > prize"), evalTrue)
    probeParseResult(p.parse("passes(phone, filter1) & ! passes(description, filter1)"), evalTrue)

    expectException(() => probeParseResult(p.parse("prize == size"), ok, echo))
    expectException(() => probeParseResult(p.parse("passes(phone, unknownfilter) & ! passes(description, filter1)"), ok, echo))
    expectException(() => probeParseResult(p.parse("x < 2"), ok, echo))
    expectException(() => probeParseResult(p.parse("1 ? 2"), ok, echo))
    expectException(() => probeParseResult(p.parse("(1 < 2 &))"), ok, echo))
    expectException(() => probeParseResult(p.parse("passes(phone, filter1) & ! passes(description, filter1, oho)"), ok, echo))
    expectException(() => probeParseResult(p.parse("passes(phone, filter1) & ! someFct(description, filter1)"), ok, echo))

  }

}

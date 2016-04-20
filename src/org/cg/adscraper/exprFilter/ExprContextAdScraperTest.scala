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

class ExprContextAdScraperSuite extends JUnitSuite {

  val values = new ScrapedValues()
  val filters = new FilterList()
  val parser = new ExprParserAdScraper(values, filters)
  val c = new ExprContextAdScraper(values, filters)
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

  @Test def testNumberConversion() {
    probeResult(c.decodeNumber("0"), EvalOk(0));
    probeResult(c.decodeNumber("0.1"), EvalOk(0.1));
    probeResult(c.decodeNumber("0,1"), EvalOk(0.1));
    probeResult(c.decodeNumber("+0.1"), EvalOk(0.1));
    probeResult(c.decodeNumber("-0.1"), EvalOk(-0.1));
    expectException(() => probeResult(c.decodeNumber("1"), EvalOk(0)));
    expectException(() => probeResult(c.decodeNumber("x"), EvalOk(0)));
    expectException(() => probeResult(c.decodeNumber("1"), EvalFail("failure")));
  }

  @Test def testCtxGetValues() {

    probeResult(c.getCtxString("phone"), EvalOk("phone"));
    expectException(() => probeResult(c.getCtxString("unknown"), ok, echo));
    probeResult(c.getCtxString("prize"), EvalOk("1.1"));
    probeResult(c.getFilter("filter1"), EvalOk(filter))
    expectException(() => probeResult(c.getFilter("a non existing filter"), ok, echo));
  }

  @Test def testPassFilter() {
    probeResult(c.evalPassFilter(Id("description"), Id("filter1")), evalFalse)
    probeResult(c.evalPassFilter(Id("phone"), Id("filter1")), evalTrue)
    expectException(() => probeResult(c.evalPassFilter(Id(unknownValueName), Id("filter1")), ok, echo))
    expectException(() => probeResult(c.evalPassFilter(Id("phone"), Id(unknownValueName)), ok, echo))
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
    
    probeResult(p.parse("1 < 2"), evalTrue)
    
    probeResult(p.parse("1 > 2"), evalFalse)
    probeResult(p.parse("! prize > 2"), evalTrue)
    probeResult(p.parse("1 < prize < 100 "), evalTrue)
    probeResult(p.parse("1 < prize < 100 & (false | true) & (1 < 0 | 1 > 0) & ! prize > prize"), evalTrue)
    probeResult(p.parse("passes(phone, filter1) & ! passes(description, filter1)"), evalTrue)
    
    expectException(() => probeResult(p.parse("prize == size"), ok, echo))
    expectException(() => probeResult(p.parse("passes(phone, unknownfilter) & ! passes(description, filter1)"), ok, echo))
    expectException(() => probeResult(p.parse("x < 2"), ok, echo))
    expectException(() => probeResult(p.parse("1 ? 2"), ok, echo))
    expectException(() => probeResult(p.parse("(1 < 2 &))"), ok, echo)) 
    expectException(() => probeResult(p.parse("passes(phone, filter1) & ! passes(description, filter1, oho)"), ok, echo))
    expectException(() => probeResult(p.parse("passes(phone, filter1) & ! someFct(description, filter1)"), ok, echo))

  }
 
}

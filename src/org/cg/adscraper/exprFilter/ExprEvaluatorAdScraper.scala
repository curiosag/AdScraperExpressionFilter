package org.cg.adscraper.exprFilter

import org.cg.ads.advalues.ScrapedValues
import org.cg.ads.filterlist.FilterList
import com.google.common.base.Optional
import org.cg.ads.advalues.ValueKind
import org.cg.ads.advalues.InterpretedValue
import collection.JavaConversions._
import org.cg.scala.expressionparser._

class ExprEvaluatorAdScraper(scraped: ScrapedValues, f: FilterList) extends BooleanEvaluator(new AdScraperEnvironment(scraped, f))

class AdScraperEnvironment(scraped: ScrapedValues, f: FilterList) extends EvalEnvironment {

  override def evalFunc(name: Id, params: List[Token]): EvalResult[Boolean] =
    {
      if (!(name.token.equals("passes") && params.length == 2 && params.foldLeft(true)((x, y) => x && y.isInstanceOf[Id]))) {
        EvalFail("invalid function definition '%s' params: %s".format(name.token, params.foldLeft("")((x, y) => x + " " + y.token)))
      } else {
        evalPassFilter(params.head, params.tail.head)
      }
    }

  override def getNumber(v: Token) =
    {
      v match {
        case Id(token) => getCtxNumber(token)
        case Num(token) => decodeNumber(token)
        case _ => EvalFail("unexpected token type")
      }
    }

  def getFilter(name: String) = {
    J2S.conv(f.get(name)) match {
      case Some(filter) => EvalOk(filter)
      case _ => EvalFail("Filter '%s' not defined".format(name))
    }
  }

  private def evalResultBoolTyped(s: String): EvalResult[Boolean] = EvalFail(s)

  def evalPassFilter(valRef: Token, filterRef: Token): EvalResult[Boolean] = {
    val ref = getCtxString(valRef.token)
    val f = getFilter(filterRef.token)

    (ref, f) match {
      case (EvalOk(ref), EvalOk(f)) => {
        EvalOk(!f.exists(x => ref.indexOf(x) > 0))
      }
      case (EvalFail(x), _) => evalResultBoolTyped(x)
      case (_, EvalFail(x)) => evalResultBoolTyped(x)
    }
  }

  def getCtxString(kindName: String) = resolveForKind(kindName, (kind) => evalValue(kindName, scraped.interpret().asString(kind)))
  def getCtxNumber(kindName: String) = resolveForKind(kindName, (kind) => {
    evalValue(kindName, scraped.interpret().asDouble(kind)) match {
      case EvalOk(dbl) => EvalOk(BigDecimal.apply(dbl.doubleValue()))
      case EvalFail(m) => EvalFail(m)
    }
  })

  private def evalValue[T](kindName: String, value: Optional[T]): EvalResult[T] = {
    J2S.conv(value) match {
      case Some(v) => EvalOk(v)
      case _ => EvalFail("Absent value for kind: " + kindName);
    }
  }

  private def resolveForKind[T](kind: String, f: (ValueKind) => EvalResult[T]): EvalResult[T] = {
    getKind(kind) match {
      case EvalOk(a) => f(a)
      case EvalFail(m) => new EvalFail(m)
    }
  }

  private def getKind(kind: String): EvalResult[ValueKind] = {
    J2S.conv(ValueKind.getValueOf(kind)) match {
      case Some(k) => new EvalOk(k)
      case _ => new EvalFail("Unknown value kind: " + kind)
    }
  }

  def decodeNumber(numValue: String): EvalResult[BigDecimal] =
    {
      try {
        EvalOk(BigDecimal.apply(numValue.replace(",", ".")))
      } catch {
        case t: NumberFormatException => EvalFail("Invalid number format: " + numValue)
        case t: Exception => EvalFail("Unexpected exception when converting " + numValue + " " + t.getClass.getSimpleName)
      }
    }

}
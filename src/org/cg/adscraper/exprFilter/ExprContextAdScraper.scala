package org.cg.adscraper.exprFilter

import org.cg.ads.advalues.ScrapedValues
import org.cg.ads.filterlist.FilterList
import com.google.common.base.Optional
import org.cg.ads.advalues.ValueKind
import org.cg.ads.advalues.InterpretedValue
import collection.JavaConversions._

class ExprContextAdScraper(scraped: ScrapedValues, f: FilterList) extends ExprEvaluator[Boolean] {

  val constDecode = Map(("true", EvalOk(true)), ("false", EvalOk(false)))
  val relOpDecode = Map(
    (">", EvalOk((x: BigDecimal, y: BigDecimal) => x > y)),
    ("<", EvalOk((x: BigDecimal, y: BigDecimal) => x < y)),
    ("<=", EvalOk((x: BigDecimal, y: BigDecimal) => x <= y)),
    (">=", EvalOk((x: BigDecimal, y: BigDecimal) => x >= y)),
    ("==", EvalOk((x: BigDecimal, y: BigDecimal) => x == y)))

  def getFilter(name: String) = {
    J2S.conv(f.get(name)) match {
      case Some(filter) => EvalOk(filter)
      case _ => EvalFail("Filter '%s' not defined".format(name))
    }
  };

  override def evalConst(const: Id): EvalResult[Boolean] =
    {
      constDecode.get(const.token) match {
        case Some(c) => c
        case _ => EvalFail("Unknown const symbol: " + const)
      }
    }

  override def evalUnOp(arg: EvalResult[Boolean], op: Op): EvalResult[Boolean] = evalBooleanNot(arg, op)

  def getNumber(v: Token) =
    {
      v match {
        case Id(token) => getCtxNumber(token)
        case Num(token) => decodeNumber(token)
        case _ => EvalFail("unexpected token type")
      }
    }

  override def evalRelOp(v1: Token, op: Op, v2: Token): EvalResult[Boolean] = {
    getNumber(v1) match {
      case EvalOk(v1) => {
        getNumber(v2) match {
          case EvalOk(v2) => {
            getRelOp(op) match {
              case EvalOk(f) => EvalOk(f(v1, v2))
              case EvalFail(m) => evalResultBoolTyped(m)
            }
          }
          case EvalFail(m) => evalResultBoolTyped(m)
        }
      }
      case EvalFail(m) => evalResultBoolTyped(m)
    }
  }

  private def evalResultBoolTyped(s: String): EvalResult[Boolean] = EvalFail(s)

  def evalBinOpBoolean(v1: EvalResult[Boolean], op: Op, v2: EvalResult[Boolean]): EvalResult[Boolean] =
    {
      (v1, getBinBooleanOp(op), v2) match {
        case (EvalOk(left), EvalOk(op), EvalOk(right)) => EvalOk(op(left, right))
        case (EvalFail(x), _, _) => EvalFail(x)
        case (_, EvalFail(x), _) => EvalFail(x)
        case (_, _, EvalFail(x)) => EvalFail(x)
      }
    }

  override def evalFunc(name: Id, params: List[Token]): EvalResult[Boolean] =
    {
      if (!(name.token.equals("passes") && params.length == 2 && params.foldLeft(true)((x, y) => x && y.isInstanceOf[Id]))) {
        EvalFail("invalid function definition '%s' params: %s".format(name.token, params.foldLeft("")((x, y) => x + " " + y.token)))
      } else {
        evalPassFilter(params.head, params.tail.head)
      }
    }

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

  private def evalOptBoolean(x: Option[Boolean], y: Option[Boolean], f: (Boolean, Boolean) => Boolean) = {
    (x, y) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }
  }

  private def getRelOp[V](op: Op) = {
    relOpDecode.get(op.token).orElse(Some(EvalFail("invalid relational operator: " + op.token))).get
  }

  private def getBinBooleanOp(op: Op) = {
    op.token match {
      case "&" => EvalOk((x: Boolean, y: Boolean) => x & y)
      case "|" => EvalOk((x: Boolean, y: Boolean) => x | y)
      case _ => EvalFail("invalid boolean operator: " + op.token)
    }
  }

  def evalBooleanNot(v: EvalResult[Boolean], op: Op): EvalResult[Boolean] = {
    if (op.token.equals("!")) {
      v match {
        case EvalOk(boolVal) => EvalOk(!boolVal)
        case x => x
      }
    } else { EvalFail("invalid boolean operator: " + op.token) }

  }

}
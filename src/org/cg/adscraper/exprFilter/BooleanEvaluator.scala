package org.cg.adscraper.exprFilter

import org.cg.ads.advalues.ScrapedValues
import org.cg.ads.filterlist.FilterList
import com.google.common.base.Optional
import org.cg.ads.advalues.ValueKind
import org.cg.ads.advalues.InterpretedValue
import collection.JavaConversions._
import org.cg.scala.expressionparser._

trait EvalEnvironment {
    def evalFunc(funcName: Id, params: List[Token]): EvalResult[Boolean]
    def getNumber(value: Token): EvalResult[BigDecimal]    
}

class BooleanEvaluator(env: EvalEnvironment) extends ExprEvaluator[EvalResult[Boolean]] {

  val constDecode = Map(("true", EvalOk(true)), ("false", EvalOk(false)))
  val relOpDecode = Map(
    (">", EvalOk((x: BigDecimal, y: BigDecimal) => x > y)),
    ("<", EvalOk((x: BigDecimal, y: BigDecimal) => x < y)),
    ("<=", EvalOk((x: BigDecimal, y: BigDecimal) => x <= y)),
    (">=", EvalOk((x: BigDecimal, y: BigDecimal) => x >= y)),
    ("==", EvalOk((x: BigDecimal, y: BigDecimal) => x == y)))

  override def evalConst(const: Id): EvalResult[Boolean] =
    {
      constDecode.get(const.token) match {
        case Some(c) => c
        case _ => EvalFail("Unknown const symbol: " + const)
      }
    }

  override def evalUnOp(arg: EvalResult[Boolean], op: Op): EvalResult[Boolean] = evalBooleanNot(arg, op)

  override def evalRelOp(v1: Token, op: Op, v2: Token): EvalResult[Boolean] = {
    env.getNumber(v1) match {
      case EvalOk(v1) => {
        env.getNumber(v2) match {
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
      env.evalFunc(name, params)
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
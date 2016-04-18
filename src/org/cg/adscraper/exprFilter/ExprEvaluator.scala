package org.cg.adscraper.exprFilter

trait ExprEvaluator[T] {
  def evalConst(const: String): EvalResult[T]
  def evalUnOp(arg: EvalResult[T], op: Op): EvalResult[T]
  def evalRelOp(val1: Token, op: Op, val2: Token): EvalResult[T]
  def evalBinOpBoolean(arg1: EvalResult[T], op: Op, arg2: EvalResult[T]): EvalResult[T]

  def evalPassFilter(valRef: Id, filterRef: Id): EvalResult[T]
}

object defaultExprEvaluator extends ExprEvaluator[String] {
  def trace(v: String) = {System.out.println(v)}

  override def evalConst(const: String): EvalResult[String] = {
    trace("evalConst: " + const)
    EvalOk(const)
  }

  override def evalUnOp(arg: EvalResult[String], op: Op): EvalResult[String] = {
    val ret = "(%s %s)".format(op.token, arg)
    trace("evalUnOp: " + ret)
    EvalOk(ret)
  }

  override def evalRelOp(val1: Token, op: Op, val2: Token): EvalResult[String] = {
    val ret = "%s %s %s".format(val1.token, op.token, val2.token)
    trace("evalRelOp: " + ret)
    EvalOk(ret)
  }

  override def evalBinOpBoolean(arg1: EvalResult[String], op: Op, arg2: EvalResult[String]): EvalResult[String] = {
    val ret = "(%s %s %s)".format(arg1, op.token, arg2)
    trace("evalBinOpBoolean: " + ret)
    EvalOk(ret)
  }

  override def evalPassFilter(value: Id, filter: Id): EvalResult[String] = {
    val ret = "passFilter: %s %s".format(value.token, filter.token)
    trace("evalPassFilter: " + ret)
    EvalOk(ret)
  }
}
package org.cg.adscraper.exprFilter

abstract class EvalResult[+T]
case class EvalOk[T](result: T) extends EvalResult[T] {
  override def toString() = result.toString()
}
case class EvalFail(msg: String) extends EvalResult[Nothing] {
  override def toString() = msg
}

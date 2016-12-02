package org.cg.adscraper.exprFilter

import org.cg.ads.advalues.ScrapedValues
import org.cg.ads.filterlist.FilterList
import org.cg.scala.expressionparser._

class ExprParserAdScraper(v: ScrapedValues, f: FilterList) extends ExprParser[EvalResult[Boolean]](new ExprEvaluatorAdScraper(v, f)) {
  
  def eval(rule: String): ResultAdScraper = {
    parse(rule) match {
      case ExprOk(EvalOk(true)) => new ResultAdScraper(1, "")
      case ExprOk(EvalOk(false)) => new ResultAdScraper(0, "")
      case ExprErr(msg) => new ResultAdScraper(-1, msg)
      case ExprOk(EvalFail(msg)) => new ResultAdScraper(-1, msg)
    }
  }
  
}
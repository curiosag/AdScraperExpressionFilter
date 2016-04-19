package org.cg.adscraper.exprFilter

import org.cg.ads.advalues.ScrapedValues
import org.cg.ads.filterlist.FilterList

class ExprParserAdScraper(v: ScrapedValues, f: FilterList) extends ExprParser[Boolean](new ExprContextAdScraper(v, f)) {
  
  def eval(rule: String): ResultAdScraper = {
    parse(rule) match {
      case EvalOk(true) => new ResultAdScraper(1, "")
      case EvalOk(false) => new ResultAdScraper(0, "")
      case EvalFail(msg) => new ResultAdScraper(-1, msg)
    }
  }
  
}
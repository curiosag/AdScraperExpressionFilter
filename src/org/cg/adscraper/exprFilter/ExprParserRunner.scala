package org.cg.adscraper.exprFilter

import org.cg.ads.advalues._;
import org.cg.ads.filterlist._;

object ExprParserRunner {
  def main(args: Array[String]) {
    val values = new ScrapedValues()
    val filters = new FilterList()

    val src = "! (!(0 < px <= 10 & a > 2 | a <= 10) & ! b > 3 & ! true & ! (false) | passes(val, filter) | c == 0)";

    new ExprParser(defaultExprEvaluator).parse(src)
    
  }
}
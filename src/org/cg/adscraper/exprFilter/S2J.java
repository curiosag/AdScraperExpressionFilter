package org.cg.adscraper.exprFilter;

import org.cg.ads.advalues.ScrapedValues;
import org.cg.ads.filterlist.FilterList;

public class S2J {

	public static ResultAdScraper evalExpr(ScrapedValues v, FilterList f, String expr)
	{
		return new ExprParserAdScraper(v, f).eval(expr);
	}
	
}

package org.cg.adscraper.exprFilter

import com.google.common.base.Optional

object J2S {
  
  def conv[T](opt: Optional[T]) =
    if (opt.isPresent())
      Some(opt.get)
    else None

}
package com.consetto.twitterprofileclassifier

import scala.io.Source

object DataLoaders {

  def loadFirstnames =
    Source.
      fromInputStream(getClass.getResourceAsStream("/firstnames.txt")).
      getLines.
      map(_.toLowerCase).
      toList

  def loadManualClassified =
    Source.
      fromInputStream(getClass.getResourceAsStream("/dataset.csv")).
      getLines.
      take(440).
      map(_.split(",").toVector).
      drop(2).
      map { case xs =>
        val profilename = xs(0)
        val clazz = xs(2).trim
        val normalizedClazz = if (clazz == "C") "C" else "P"
        (profilename, normalizedClazz)
      }
}

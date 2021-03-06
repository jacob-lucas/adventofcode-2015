package com.jacoblucas.adventofcode2015

import scala.io.Source

object Util {
  def read(filename: String): List[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream(filename))
      .mkString
      .split("\n")
      .toList
}

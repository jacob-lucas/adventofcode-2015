package com.jacoblucas.adventofcode2015

import scala.io.Source

object Day02 {

  def surfaceArea(len: Int, wid: Int, hgt: Int): Int = 2*len*wid + 2*wid*hgt + 2*hgt*len

  def smallestSideArea(len: Int, wid: Int, hgt: Int): Int = {
    val a = len * wid
    val b = wid * hgt
    val c = len * hgt

    math.min(math.min(a, b), math.min(b, c))
  }

  def paperNeeded(dim: (Int, Int, Int)): Int =
    surfaceArea(dim._1, dim._2, dim._3) + smallestSideArea(dim._1, dim._2, dim._3)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day02-input.txt")).mkString.split("\n")
    val dimensions = lines.map(str => str.split("x").map(_.toInt))

    val total = dimensions
      .map(d => paperNeeded((d(0), d(1), d(2))))
      .sum

    println(total)
  }

}

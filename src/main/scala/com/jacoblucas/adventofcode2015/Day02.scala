package com.jacoblucas.adventofcode2015

import scala.collection.mutable.ArrayBuffer

object Day02 {

  def surfaceArea(len: Int, wid: Int, hgt: Int): Int = 2*len*wid + 2*wid*hgt + 2*hgt*len

  def smallestSideArea(len: Int, wid: Int, hgt: Int): Int = Array(len * wid, wid * hgt, len * hgt).min

  def paperNeeded(dim: (Int, Int, Int)): Int =
    surfaceArea(dim._1, dim._2, dim._3) + smallestSideArea(dim._1, dim._2, dim._3)

  def ribbonNeeded(dim: (Int, Int, Int)): Int = {
    val arr = ArrayBuffer(dim._1, dim._2, dim._3)
    val min = arr.min
    arr -= min
    val nextMin = arr.min

    val box = min + min + nextMin + nextMin
    val bow = dim._1 * dim._2 * dim._3
    box + bow
  }

  def main(args: Array[String]): Unit = {
    val lines = Util.read("/day02-input.txt")
    val dimensions = lines.map(str => str.split("x").map(_.toInt))

    val total = dimensions
      .map(d => paperNeeded((d(0), d(1), d(2))))
      .sum

    println(total)

    val ribbon = dimensions
      .map(d => ribbonNeeded((d(0), d(1), d(2))))
      .sum

    println(ribbon)
  }

}

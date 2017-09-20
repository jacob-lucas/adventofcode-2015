package com.jacoblucas.adventofcode2015

import scala.annotation.tailrec
import scala.io.Source

object Day05 {

  def containsThreeVowels(str: String): Boolean =
    List[Char]('a', 'e', 'i', 'o', 'u')
      .map(v => str.count(_ == v))
      .sum >= 3

  @tailrec
  def containsRepeatingLetter(str: String): Boolean =
    str.toList match {
      case x :: y :: xs => if (x == y) true else containsRepeatingLetter((List(y) ++ xs).mkString)
      case _ => false
    }

  def containsSubstring(str: String): Boolean =
    List[String]("ab", "cd", "pq", "xy").exists(str.contains(_))

  def isNice(str: String): Boolean =
    containsThreeVowels(str) && containsRepeatingLetter(str) && !containsSubstring(str)

  def matcher(pattern: String): String => Boolean = pattern.r.unanchored.findFirstIn(_).isDefined
  def repeatWithOneLetterBetween: String => Boolean = matcher("""(\w)\w\1""")
  def pairAppearsTwice: String => Boolean = matcher("""(\w)(\w).*\1\2""")
  def isNiceV2(str: String): Boolean = pairAppearsTwice(str) && repeatWithOneLetterBetween(str)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day05-input.txt")).mkString.split("\n")
    println(lines count isNice)
    println(lines count isNiceV2)
  }

}

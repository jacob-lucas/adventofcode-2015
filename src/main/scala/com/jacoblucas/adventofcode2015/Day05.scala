package com.jacoblucas.adventofcode2015

import scala.annotation.tailrec
import scala.io.Source

object Day05 {

  def containsThreeVowels(str: String): Boolean = {
    List[Char]('a', 'e', 'i', 'o', 'u')
      .map(v => str.count(_ == v))
      .sum >= 3
  }

  @tailrec
  def containsRepeatingLetter(str: String): Boolean = {
    str.toList match {
      case x :: y :: xs => if (x == y) true else containsRepeatingLetter((List(y) ++ xs).mkString)
      case x :: Nil => false
      case Nil => false
    }
  }

  def containsSubstring(str: String): Boolean =
    List[String]("ab", "cd", "pq", "xy").exists(str.contains(_))

  def isNice(str: String): Boolean =
    containsThreeVowels(str) && containsRepeatingLetter(str) && !containsSubstring(str)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day05-input.txt")).mkString.split("\n")

//    println(isNice("ugknbfddgicrmopn"))
//    println(isNice("aaa"))
//    println(!isNice("jchzalrnumimnmhp"))
//    println(!isNice("haegwjzuvuyypxyu"))
//    println(!isNice("dvszwmarrgswjxmb"))

    println(lines count isNice)
  }

}

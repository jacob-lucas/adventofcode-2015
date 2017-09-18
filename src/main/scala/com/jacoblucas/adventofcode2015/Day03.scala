package com.jacoblucas.adventofcode2015

import scala.annotation.tailrec
import scala.io.Source

object Day03 {

  // x, y coordinates -> count
  val presents: Map[(Int, Int), Int] = Map(((0,0), 1))

  @tailrec
  def deliver(instructions: List[Char], presents: Map[(Int, Int), Int], currentLoc: (Int, Int)): Map[(Int, Int), Int] = {
    instructions match {
      case x :: xs =>
        val newLoc = x match {
          case '>' => (currentLoc._1 + 1, currentLoc._2) // x + 1
          case '<' => (currentLoc._1 - 1, currentLoc._2) // x - 1
          case '^' => (currentLoc._1, currentLoc._2 + 1) // y + 1
          case 'v' => (currentLoc._1, currentLoc._2 - 1) // y - 1
        }

        val updated = presents.get(newLoc) match {
          case Some(n) => presents.updated(newLoc, n + 1)
          case None => presents + (newLoc -> 1)
        }

        deliver(xs, updated, newLoc)
      case Nil => presents
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day03-input.txt")).mkString
    val result = deliver(lines.toList, presents, (0,0))
    println(result.size)
  }

}

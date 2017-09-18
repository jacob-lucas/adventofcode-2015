package com.jacoblucas.adventofcode2015

import scala.annotation.tailrec
import scala.io.Source

object Day03 {

  // x, y coordinates -> count
  val presents: Map[(Int, Int), Int] = Map(((0,0), 2))

  def deliver(loc: (Int, Int), presents: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
    presents.get(loc) match {
      case Some(n) => presents.updated(loc, n + 1)
      case None => presents + (loc -> 1)
    }

  def getNewLocation(currentLoc: (Int, Int), x: Char): (Int, Int) =
    x match {
      case '>' => (currentLoc._1 + 1, currentLoc._2) // x + 1
      case '<' => (currentLoc._1 - 1, currentLoc._2) // x - 1
      case '^' => (currentLoc._1, currentLoc._2 + 1) // y + 1
      case 'v' => (currentLoc._1, currentLoc._2 - 1) // y - 1
    }

  @tailrec
  def deliver(instructions: List[Char], presents: Map[(Int, Int), Int], currentSantaLoc: (Int, Int), currentRoboSantaLoc: (Int, Int), n: Int): Map[(Int, Int), Int] = {
    instructions match {
      case x :: xs =>
        if (n % 2 == 0) {
          val newLoc = getNewLocation(currentRoboSantaLoc, x)
          deliver(xs, deliver(newLoc, presents), currentSantaLoc, newLoc, n + 1)
        } else {
          val newLoc = getNewLocation(currentSantaLoc, x)
          deliver(xs, deliver(newLoc, presents), newLoc, currentRoboSantaLoc, n + 1)
        }
      case Nil => presents
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day03-input.txt")).mkString
    val result = deliver(lines.toList, presents, (0,0), (0,0), 1)
    println(result.size)
  }

}

package com.jacoblucas.adventofcode2015

import scala.annotation.tailrec
import scala.io.Source

object Day01 {

  @tailrec
  def followInstructions(instructions: String, floor: Int): Int = {
    instructions.toList match {
      case x :: xs if x == '(' => followInstructions(xs.mkString, floor + 1)
      case x :: xs if x == ')' => followInstructions(xs.mkString, floor - 1)
      case Nil => floor
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/day01-input.txt")).mkString
    println(followInstructions(lines, 0))
  }

}

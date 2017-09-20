package com.jacoblucas.adventofcode2015

import scala.annotation.tailrec

object Day01 {

  @tailrec
  def followInstructions(instructions: String, floor: Int): Int = {
    instructions.toList match {
      case x :: xs if x == '(' => followInstructions(xs.mkString, floor + 1)
      case x :: xs if x == ')' => followInstructions(xs.mkString, floor - 1)
      case Nil => floor
    }
  }

  @tailrec
  def basementChar(instructions: String, floor: Int, pos: Int): Int = {
    instructions.toList match {
      case x :: xs =>
        if (floor == -1) pos - 1
        else {
          if (x == '(') basementChar(xs.mkString, floor + 1, pos + 1)
          else basementChar(xs.mkString, floor - 1, pos + 1)
        }
      case Nil => 0
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Util.read("/day01-input.txt").head
    println(followInstructions(lines, 0))
    println(basementChar(lines, 0, 1))
  }

}

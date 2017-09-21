package com.jacoblucas.adventofcode2015

trait Operation
case object On extends Operation
case object Off extends Operation
case object Toggle extends Operation

case class Instruction(operation: Operation, start: (Int, Int), end: (Int, Int)) {
  override def toString: String = operation + ": " + start + " -> " + end
}

object Day06 {

  def parse(str: String): Instruction = {
    val parts = str.split(" ")
    val startStr = parts(parts.size - 3).split(",").map(_.toInt)
    val endStr = parts(parts.size - 1).split(",").map(_.toInt)

    val start = (startStr(0), startStr(1))
    val end = (endStr(0), endStr(1))
    val op = parts(0) match {
      case "toggle" => Toggle
      case _ =>
        parts(1) match {
          case "on" => On
          case "off" => Off
        }
    }

    Instruction(op, start, end)
  }

  def execute(inst: Instruction, grid: Array[Array[Int]]): Unit = {
    val topLeft = inst.start
    val bottomRight = inst.end

    for {
      i <- topLeft._1 to bottomRight._1
      j <- topLeft._2 to bottomRight._2
    } {
      inst.operation match {
        case On => grid(j)(i) = 1
        case Off => grid(j)(i) = 0
        case Toggle => if (grid(j)(i) == 0) grid(j)(i) = 1 else grid(j)(i) = 0
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Util.read("/day06-input.txt")
    val instructions = lines map parse

    val grid = Array.ofDim[Int](1000, 1000)
    instructions.foreach(i => execute(i, grid))

    println(grid.map(_.count(_ == 1)).sum)
  }

}

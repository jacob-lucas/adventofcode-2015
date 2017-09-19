package com.jacoblucas.adventofcode2015

import java.security.MessageDigest

import scala.annotation.tailrec

object Day04 {

  @tailrec
  def findLowestPositiveNumber(secretKey: String, n: Int): Int = {
    val str = secretKey + n.toString
    val md5 = MessageDigest
      .getInstance("MD5")
      .digest(str.getBytes)
      .map("%02X".format(_))
      .mkString

    println(n + " = " + md5)
    if (md5.startsWith("00000")) n
    else findLowestPositiveNumber(secretKey, n + 1)
  }

  def main(args: Array[String]): Unit = {
    println(findLowestPositiveNumber("yzbqklnj", 0))
  }
}

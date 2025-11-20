package com.brandon.scala.mcqs

object Q11 {

  def main(args: Array[String]): Unit = {
    val a = Array(1, 2, 3, 4)

    val as =  for {e <- a} yield {
      val eOne = e + 1
      val e2 = eOne + 1 }
  }

}

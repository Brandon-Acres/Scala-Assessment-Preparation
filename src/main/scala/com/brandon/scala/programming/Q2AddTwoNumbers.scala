package com.brandon.scala.programming

object Q2AddTwoNumbers {
  /*
  2. Add Two Numbers
Medium
You are given two non-empty linked lists representing two non-negative integers.
The digits are stored in reverse order, and each of their nodes contains a single digit.
Add the two numbers and return the sum as a linked list.
You may assume the two numbers do not contain any leading zero, except the number 0 itself.

   */

  // approach
  // define singly linked list - define nodes and end node.

  // approach to summation
  // method 1: manually add each digit and carry forward remainder to future values
  // method 2: parse each list into an integer, add the integers, and parse the result back into a linkedList.

  // I will go with method 2 for simplicity now

  // plan:
  // Scala's List type already acts like a singly linked list with head, tails and isEmpty
  // so we need
  // addTwoNumbers(list1, list2): List[Int] => does the actual work
  // helper functions:
  // listToInt(list)
  // IntToList(list)

  // constraint: since number of elements can be up to 100, more than capacity of long or int.
  // so need to use element-wise addition.

  def addTwoNumbers(list1: List[Int], list2: List[Int], carry: Int): List[Int] = {
    // we need to build up in reverse order: so we deal with  10^0 digit first, then up to 10^n
    // partialSum = addTwoNumbers(list1.tails, list2.tails)
    // we build list iteratively with (list1 digit + list2 digit) % 10 :: ((list1 digit + list2 digit) / 10) + partialSum.head :: partialSum.tails

    list1 match {
      case Nil => {
         list2 match {
           // base case:
           // both lists are Nil return Nil - so that we can build list with ::
          case Nil => if (carry > 0) carry :: Nil else Nil

          // recursive case:
          // list2 has head :: tail, list 1 is Nil (use list2 value, persist Nil for list 1)
          // we may have remainder.
          case head2 :: tail2 => {
            val newDigit = (head2 + carry) % 10
            val newCarry = (head2 + carry) / 10
            newDigit :: addTwoNumbers(Nil, tail2, newCarry)
          }
        }
      }
      case head1 :: tail1 => {
        list2 match {
          // recursive case:
          // list 1 is nonempty, list2 is empty
          // use partial sum addTwoNumbers(tail1, Nil)
          // create list head1 :: partialSum
          case Nil => {
            val newDigit = (head1 + carry) % 10
            val newCarry = (head1 + carry) / 10
            newDigit :: addTwoNumbers(tail1, Nil, newCarry)
          }

          // recursive case
          // both lists are non-empty
          // sum digits, append remainder % 10 to head of partial sum
          // if sum > 10, add an extra 1 to the head of the partial sum
          case head2 :: tail2 => {
            val newDigit = (head1 + head2 + carry) % 10
            val newCarry = (head1 + head2 + carry) / 10
            newDigit :: addTwoNumbers(tail1, tail2, newCarry)
          }
        }
      }
    }
  }

  // test:
  def main(args: Array[String]): Unit = {
    val l1a = List(2, 4, 3)
    val l1b = List(5, 6, 4)
    // should give List(7, 0, 8)
    printTestCase(l1a, l1b, List(7, 0, 8))

    val l2a = List(0)
    val l2b = List(0)
    printTestCase(l2a, l2b, List(0))

    val l3a = List(9, 9, 9, 9, 9, 9, 9)
    val l3b = List(9, 9, 9, 9)
    printTestCase(l3a, l3b, List(8, 9, 9, 9, 0, 0, 0, 1))
    // fails - we need to take into account a carry at each recursive call
  }

  def printTestCase(list1: List[Int], list2: List[Int], expected: List[Int]): Unit = {
    println("--- Test case ---")
    println(s"List1: $list1")
    println(s"List2: $list2")
    println(s"Expected result: $expected")
    println(s"Actual result: ${addTwoNumbers(list1, list2, 0)}")
  }
}

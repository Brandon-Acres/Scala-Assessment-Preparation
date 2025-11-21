package com.brandon.scala.programming

object Q4MedianOfTwoSortedArrays {

  /*
   * 4. Median of Two Sorted Arrays
Hard
Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays.
The overall run time complexity should be O(log (m+n)).
Example 1:
Input: nums1 = [1,3], nums2 = [2]
Output: 2.00000
Explanation: merged array = [1,2,3] and median is 2.
Example 2:
Input: nums1 = [1,2], nums2 = [3,4]
Output: 2.50000
Explanation: merged array = [1,2,3,4] and median is (2 + 3) / 2 = 2.5.
Example 3:
Input: nums1 = [0,0], nums2 = [0,0]
Output: 0.00000
Example 4:
Input: nums1 = [], nums2 = [1]
Output: 1.00000
Example 5:
Input: nums1 = [2], nums2 = []
Output: 2.00000
Constraints:
nums1.length == m
nums2.length == n
0 <= m <= 1000
0 <= n <= 1000
1 <= m + n <= 2000
-106 <= nums1[i], nums2[i] <= 106
   */

  // approach - we can't combine the arrays, as that is O(n+m) which is already too great.
  // so we must do a search on the elements within their arrays.

  // edge cases:
  // if one of the arrays are empty: then median is the median of the other array (just find middle element or two middle / 2)

  // how can we do a binary search on the two lists without merging them?
  // we are given only the arrays - can we assume we know their length? yes otherwise wouldn't be able to find m/2 or n/2

  // answer:
  // we binary search over the index of smaller array
  // find position in both arrays such that size of a_left + b_left == a_right + b_right
  // and check that conditions
  // a_left_max <= b_right_min
  // a_right_min >= b_left_max

  // if not, we reduce window accordingly
  // solution:


  def findMedianSortedArray(a_in: Array[Int], b_in: Array[Int]): Double = {
    // ensure a is smaller
    val (a, b) = if (a_in.length <= b_in.length) (a_in, b_in) else (b_in, a_in)
    val m = a.length
    val n = b.length

    // define binary search window across a - which we start with full a
    var low = 0
    var high = m
    val totalLeft = (m + n + 1) / 2

    while (low <= high) {
      val i = (low + high) / 2 // integer div, total in left of a
      val j = totalLeft - i // total in left of b

      // find corresponding maxes of left and mins of right paritions of arrays
      val a_left_max = if (i == 0) Int.MinValue else a(i - 1)
      val a_right_min = if (i == m) Int.MaxValue else a(i)

      val b_left_max = if (j == 0) Int.MinValue else b(j - 1)
      val b_right_min = if (j == n) Int.MaxValue else b(j)

      if (a_left_max <= b_right_min && a_right_min >= b_left_max) {
        // found correct partition, find median of max of left and min of right
        if ((m + n) % 2 == 1) {
          // odd case
          return math.max(a_left_max, b_left_max)
        } else {
          // even case
          return (math.max(a_left_max, b_left_max) + math.min(a_right_min, b_right_min)) / 2.0
        }

      }
      else if (a_left_max > b_right_min) {
        high = i - 1 // too many values from a.
      }
      else {
        // too few from a
        low = i + 1
      }
    }

    throw new IllegalStateException("No median found") // shouldn't happen
  }

  def main(args: Array[String]): Unit = {

    printMedian(Array(1, 3), Array(2), 2.0)
    printMedian(Array(1, 2), Array(3, 4), 2.5)
    printMedian(Array(0, 0), Array(0, 0), 0.0)
    printMedian(Array(), Array(1), 1.0)
    printMedian(Array(2), Array(), 2.0)

  }

  def printMedian(arr1: Array[Int], arr2: Array[Int], expected: Double): Unit = {
    println("--- Test case ---")
    println(s"arr1: ${arr1.mkString("[", ", ", "]")}")
    println(s"arr1: ${arr2.mkString("[", ", ", "]")}")
    println(s"Expected median: $expected")
    println(s"Actual median: ${findMedianSortedArray(arr1, arr2)}")
  }



}

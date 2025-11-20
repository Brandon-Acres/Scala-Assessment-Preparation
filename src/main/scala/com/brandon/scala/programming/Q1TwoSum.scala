package com.brandon.scala.programming

object Q1TwoSum {
  /*
   * 1. Two Sum
    Easy
    Given an array of integers nums and an integer target, return indices of the two numbers such that
    they add up to target.
    You may assume that each input would have exactly one solution, and you may not use the same element twice.
    You can return the answer in any order.
    Example 1:
    Input: nums = [2,7,11,15], target = 9
    Output: [0,1]
    Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].
    Example 2:
    Input: nums = [3,2,4], target = 6
    Output: [1,2]
    Example 3:
    Input: nums = [3,3], target = 6
    Output: [0,1]
    Constraints:
    2 <= nums.length <= 104
    -109 <= nums[i] <= 109
    -109 <= target <= 109
    Only one valid answer exists.
    Follow-up: Can you come up with an algorithm that is less than O(n2) time complexity?
   */

  def main(args: Array[String]): Unit = {

    // example inputs
    val nums1 = Array(2, 7, 11, 15)
    val target1 = 9
    printTwoSumResult(nums1, target1)

    val nums2 = Array(3, 2, 4)
    val target2 = 6
    printTwoSumResult(nums2, target2)

    val nums3 = Array(3, 3)
    val target3 = 6
    printTwoSumResult(nums3, target3)
  }
  

  def printTwoSumResult(nums: Array[Int], target: Int): Unit = {
    println(s"--- Two Sum ---")
    println(s"Input: ${nums.mkString("[", " ", "]")}")
    println(s"Target: $target")
    println(s"Two Sum: ${twoSumIndices(nums, target)}")
  }
  

  // don't use fold first because we want early collection:
  def twoSumIndices(nums: Array[Int], target: Int): (Int, Int) = {
    // create map with values pointing to their indices (list if more than one occurs)
    val valueIndexMap: Map[Int, List[Int]] = nums.indices.groupBy(i => nums(i)).map((value, indices) => (value, indices.toList)) // maintains collection of indices for same value

    // successful pair of indices
    // case 1: (target - value) == value -> then if there is another unique index with same value, we can create valid pair, otherwise it isn't valid
    // case 2: (target - value) != value -> then if valueIndexMap contains key (target - value),
    // return head of the list it points to (first index that points to the right value (target - value))
    valueIndexMap.collectFirst {
      case (value, indices) if ((target - value) == value && indices.size >= 2) => (indices.head, indices(1))
      case (value, indices) if ((target - value) != value && valueIndexMap.contains(target - value)) =>
        (indices.head, valueIndexMap.getOrElse(target - value, List(-1)).head)
    }.getOrElse((-1, -1))
  }

}

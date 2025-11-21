package com.brandon.scala.programming

object Q3MaxSubString {

  /*
  3. Longest Substring Without Repeating Characters
Medium
Given a string s, find the length of the longest substring without repeating characters.
Example 1:
Input: s = "abcabcbb"
Output: 3
Explanation: The answer is "abc", with the length of 3.
Example 2:
Input: s = "bbbbb"
Output: 1
Explanation: The answer is "b", with the length of 1.
Example 3:
Input: s = "pwwkew"
Output: 3
Explanation: The answer is "wke", with the length of 3. Notice that the answer must be a substring, "pwke" is a subsequence and not a substring.
Example 4:
Input: s = ""
Output: 0
Constraints:
	• 0 <= s.length <= 5 * 104
s consists of English letters, digits, symbols and spaces.
   */


  // solution, use a fold to accumulate the max substring as well as the current substring being built with no
  // repeated characters:

  // returns the max substring
  def maxSubString(text: String): String = {
    text.foldLeft(("", ""): (String, String)) {
      case ((maxSubString, currentSubString), c)
      =>
      val newCurrentSubString: String = if (!currentSubString.contains(c)) currentSubString + c
      else currentSubString.substring(currentSubString.indexOf(c) + 1) + c // starts new substring from first character
      // after previous occurrence of this character
      val newMaxSubString: String = if (newCurrentSubString.length > maxSubString.length) newCurrentSubString else maxSubString
      (newMaxSubString, newCurrentSubString)
    }._1
  }

  def main(args: Array[String]): Unit = {
    val inputs = Array("abcabcbb", "bbbbb", "pwwkew", "")
    inputs.foreach(printMaxSubStringAnswer)

  }

  def printMaxSubStringAnswer(text: String): Unit = {
    println("--- Test Case ---")
    println(s"Input: $text")
    println(s"Longest substring with no repeating characters:")
    val maxSubStringText = maxSubString(text)
    println(s"${maxSubStringText}")
    println(s"Length: ${maxSubStringText.length}")
  }

  // time complexity: O(n^2) - because of call to currentSubString.contains() and indexOf() is O(k) and k <= n.
  // to reduce these calls to O(1) time, we would need to store a map of the indexes that each character last occurred
  // in (or null if not in map). We also use a min and max window represented by start and end indices rather than
  // accumulating the actual substrings. Then we check that the las occurrences of the current character is not within the window.

  // space complexity: O(n) - worst case stores 2*length of string
}

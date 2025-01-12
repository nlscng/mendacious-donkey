package com.mendaciousdonkey

import scala.annotation.tailrec

object FirstRecurringCharacter {

  // Given a string, return the first recurring character in it, or null if there is no recurring character.
  // For example, given the string "acbbac", return "b". Given the string "abcdef", return null.

  private def firstRecurringCharacter(s: String): Option[Char] =
    if (s.isEmpty) None
    else if (s.length == 1) None
    else {

      @tailrec
      def findFirstRecurring(idx: Int, word: String, seen: Set[Char]): Option[Char] =
        if (idx == word.length) None
        else if (seen.contains(word.charAt(idx))) Some(word.charAt(idx))
        else findFirstRecurring(idx + 1, word, seen + word.charAt(idx))

      findFirstRecurring(0, s, Set[Char]())
    }

  @main
  def runTests(): Unit =
    val test1 = "a"
    val exp1 = None
    val out1 = firstRecurringCharacter(test1)
    println(s"Expect: ${exp1}, got: ${out1}")

    val test2 = ""
    val exp2 = None
    val out2 = firstRecurringCharacter(test2)
    println(s"Expect: ${exp2}, got: ${out2}")

    val test3 = "acbbca"
    val exp3 = 'b'
    val out3 = firstRecurringCharacter(test3)
    println(s"Expect: ${exp3}, got: ${out3}")

    val test4 = "sumtingwong"
    val exp4 = 'n'
    val out4 = firstRecurringCharacter(test4)
    println(s"Expect: ${exp4}, got: ${out4}")
}

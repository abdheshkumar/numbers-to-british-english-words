package com.abtechsoft

import com.abtechsoft.NumberToBritishEnglishWords._

class NumberToBritishEnglishWords extends NumberToWords {

  /**
    * This function will convert number to british english words.
    * This function will grouped number by 3 from right to left and zip each group with scale(e.g thousand,million etc)
    * then finally combine english words of each group by `and` and comma(,)
    *
    * @param number : Long input number
    * @return String british english words of given input number
    */
  override def numberToEnglishWords(number: Long): String = {
    val positiveNumber = math.abs(number)
    val groupsFromRightEnd = positiveNumber.toString.reverse
      .grouped(3)
      .toList
      .map(_.reverse.toLong)

    val englishWords = groupsFromRightEnd
      .zip(scales)
      .filter(_._1 > 0)
      .map {
        case (group, scaleStr) =>
          s"${numberWithThreeDigitGroupToWords(group)} $scaleStr"
      }
    englishWords.reverse.mkString(", ").trim
  }

  /**
    * This function calculate quotient and remender.
    *
    * @param number   : Long input number
    * @param devideBy : Long
    * @return (Long, Long) quotient and remender
    */
  private def extractDigitsBy(number: Long, devideBy: Long): (Long, Long) =
    (number / devideBy, number % devideBy)

  /**
    * Function will convert three or less than three digits number into british english words
    *
    * @param number : Long
    * @return String
    */
  private def numberWithThreeDigitGroupToWords(number: Long): String = {
    val (hundreds, tensAndOnes) = extractDigitsBy(number, HUNDRED)
    val (tens, ones) = extractDigitsBy(tensAndOnes, TEN)

    val hundredsStr =
      if (tensAndOnes == ZERO) numNames.get(hundreds).map(_ + " hundred")
      else if (hundreds > ZERO) numNames.get(hundreds).map(_ + " hundred and")
      else None

    val restStr = numNames.get(tensAndOnes) match {
      case Some(tn) => List(Option(tn))
      case None     => List(tensNames.get(tens), numNames.get(ones))
    }

    (List(hundredsStr) ++ restStr).flatten.mkString(" ")
  }

}

object NumberToBritishEnglishWords {
  val numbersWithTens = List("ten",
                             "twenty",
                             "thirty",
                             "forty",
                             "fifty",
                             "sixty",
                             "seventy",
                             "eighty",
                             "ninety")
  val numbersBelowTwenty = List(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen"
  )
  val tensNames: Map[Long, String] = (1L to 9L).zip(numbersWithTens).toMap
  val numNames: Map[Long, String] = (1L to 19L).zip(numbersBelowTwenty).toMap

  val scales = List("", "thousand", "million", "billion", "trillion")
  val ZERO = 0
  val TEN = 10
  val HUNDRED = 100
}

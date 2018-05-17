package com.abtechsoft

import org.scalatest.{FlatSpec, Matchers}

class NumberToBritishEnglishWordsSpec extends FlatSpec with Matchers {

  trait Fixture {
    val converter = new NumberToBritishEnglishWords()
  }

  "numberToEnglishWords" should "convert below 10 number to british english words" in new Fixture {
    converter
      .numberToEnglishWords(1) shouldBe "one"

    converter
      .numberToEnglishWords(9) shouldBe "nine"
  }

  it should "convert any below 20 number to british english words" in new Fixture {
    converter
      .numberToEnglishWords(11) shouldBe "eleven"

    converter
      .numberToEnglishWords(12) shouldBe "twelve"
  }

  it should "convert any below thousand number to british english words" in new Fixture {
    converter
      .numberToEnglishWords(102) shouldBe "one hundred and two"

    converter
      .numberToEnglishWords(105) shouldBe "one hundred and five"

    converter
      .numberToEnglishWords(100) shouldBe "one hundred"
  }

  it should "convert any number to british english words" in new Fixture {
    converter
      .numberToEnglishWords(5678) shouldBe "five thousand, six hundred and seventy eight"
    converter
      .numberToEnglishWords(788989) shouldBe "seven hundred and eighty eight thousand, nine hundred and eighty nine"

    converter
      .numberToEnglishWords(56945781) shouldBe "fifty six million, nine hundred and forty five thousand, seven hundred and eighty one"
  }
}

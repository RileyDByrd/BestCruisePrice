package byrd.riley.bestcruiseprice

import BestPrice.{CabinPrice, Rate}
import InputGathering.{readCabinPriceInput, readRateInput}
import SpecValues.{exampleCabinPrices, exampleRates}

import cats.data.NonEmptySeq
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.StringReader
import java.math.BigDecimal

class InputGatheringSpec extends AnyWordSpec with Matchers:
  protected val validRateInput = "Rate(M1, Military)\n\n"
  protected val validRateSeq: NonEmptySeq[Rate] = NonEmptySeq.one(Rate("M1", "Military"))

  protected val validCabinPriceInput = "CabinPrice(CA, M1, 200.00)\n\n"
  protected val validCabinPriceSeq: NonEmptySeq[CabinPrice] = NonEmptySeq.one(CabinPrice("CA", "M1", new BigDecimal("200.00")))

  "readRateInput" must :
    "read a Rate" when :
      "a valid Rate is input" in :
        Console.withIn(new StringReader(validRateInput)):
          readRateInput mustBe validRateSeq

      "a CabinPrice is input then a Rate is input" in :
        Console.withIn(new StringReader(s"CabinPrice(CA, M1, 200.00)\n$validRateInput")):
          readRateInput mustBe validRateSeq

      "a paren is input in the wrong place and is then input in the right place" in :
        Console.withIn(new StringReader(s"Rate(M1, Militar)y\n$validRateInput")):
          readRateInput mustBe validRateSeq

      "enter is pressed with no input then a Rate is input" in :
        Console.withIn(new StringReader(s"\n$validRateInput")):
          readRateInput mustBe validRateSeq

    "read Rates" when :
      "the example input is input" in :
        val exampleInput =
          """
            |Rate(M1, Military)
            |Rate(M2, Military)
            |Rate(S1, Senior)
            |Rate(S2, Senior)
            |""".stripMargin.appendedAll("\n\n")

        Console.withIn(new StringReader(exampleInput)):
          readRateInput mustBe exampleRates

  "readCabinPriceInput" must :
    "read a CabinPrice" when :
      "a valid CabinPrice is input" in :
        Console.withIn(new StringReader(validCabinPriceInput)):
          readCabinPriceInput mustBe validCabinPriceSeq

      "a Rate is input then a CabinPrice is input" in :
        Console.withIn(new StringReader(s"Rate(M1, Military)\n$validCabinPriceInput")):
          readCabinPriceInput mustBe validCabinPriceSeq

      "a paren is input in the wrong place and is then input in the right place" in :
        Console.withIn(new StringReader(s"CabinPrice(CA, M1, 200.0)0\n$validCabinPriceInput")):
          readCabinPriceInput mustBe validCabinPriceSeq

      "enter is pressed with no input then a Rate is input" in :
        Console.withIn(new StringReader(s"\n$validCabinPriceInput")):
          readCabinPriceInput mustBe validCabinPriceSeq

    "read CabinPrices" when :
      "the example input is input" in :
        val exampleInput =
          """
            |CabinPrice(CA, M1, 200.00)
            |CabinPrice(CA, M2, 250.00)
            |CabinPrice(CA, S1, 225.00)
            |CabinPrice(CA, S2, 260.00)
            |CabinPrice(CB, M1, 230.00)
            |CabinPrice(CB, M2, 260.00)
            |CabinPrice(CB, S1, 245.00)
            |CabinPrice(CB, S2, 270.00)
            |""".stripMargin.appendedAll("\n\n")

        Console.withIn(new StringReader(exampleInput)):
          readCabinPriceInput mustBe exampleCabinPrices
        
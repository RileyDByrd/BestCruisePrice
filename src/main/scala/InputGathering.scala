package byrd.riley.bestcruiseprice

import BestPrice.{CabinPrice, Rate}

import cats.data.NonEmptySeq

import java.math.BigDecimal
import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Try

object InputGathering:
  // Create trait with unapply so that extractors may be passed to readInput method.
  trait StringExtractor[A]:
    def unapply(rawString: String): Option[A]

  // Extract values from input and create instance of Rate if matching format of input provided in Exercise directions.
  object RateExtractor extends StringExtractor[Rate]:
    def unapply(rawRate: String): Option[Rate] = rawRate match
      case s"Rate($rateCode, $rateGroup)" => Some(Rate(rateCode, rateGroup))
      case other => None

  // Extract value as BigDecimal if can be converted to BigDecimal.  Otherwise, consider non-matching.
  private object BigDecimalExtractor:
    def unapply(rawBigDecimal: String): Option[BigDecimal] = Try(new BigDecimal(rawBigDecimal)).toOption

  // Extract values from input and create instance of CabinPrice if matching format of input provided in Exercise directions.
  object CabinPriceExtractor extends StringExtractor[CabinPrice]:
    def unapply(rawCabinPrice: String): Option[CabinPrice] = rawCabinPrice match
      case s"CabinPrice($cabinCode, $rateCode, ${BigDecimalExtractor(price)})" => Some(CabinPrice(cabinCode, rateCode, price))
      case other => None

  /**
   * Read input until at least one value of type A may be extracted.
   *
   * @param inputName     The name of the input type to display in error messages.
   * @param extractor     The extractor for the value you want.
   * @param example       An example of good input to display to the user when input is wrongly formatted.
   * @param previousInput A Seq that the method will append to until the user has finished inputting.
   * @tparam A The type of the value(s) that one wants to read.
   * @return A Seq containing at least one value of type A.
   */
  // I could have used Seq as the return type or written my own definition for a NonEmptySeq.  I used Cats NonEmptySeq
  // here because I think being as verbose with one's types as possible provides the greatest convenience to callers
  // and using the Cats ecosystem encourages interoperability with other Scala projects.
  @tailrec
  private def readInput[A](inputName: String, extractor: StringExtractor[A], example: String, previousInput: Seq[A] = Nil): NonEmptySeq[A] =
    readLine() match
      case extractor(extractedValue) => readInput(inputName, extractor, example, previousInput :+ extractedValue)
      // Don't consider input of only whitespace legitimate.
      case nothing if nothing.isBlank =>
        NonEmptySeq.fromSeq(previousInput) match
          case Some(gatheredInput) => gatheredInput
          case None =>
            println(s"You must enter at least one $inputName.")
            readInput(inputName, extractor, example, previousInput)
      case other =>
        println(s"""Input "$other" did not match expected input format.  Example:  "$example"""")
        readInput(inputName, extractor, example, previousInput)

  def readRateInput: NonEmptySeq[Rate] =
    println("Input - Rates (press enter on empty line when done):")
    readInput(inputName = "Rate", extractor = RateExtractor, example = "Rate(M1, Military)")

  def readCabinPriceInput: NonEmptySeq[CabinPrice] =
    println("Input - CabinPrices (press enter on empty line when done):")
    readInput(inputName = "CabinPrice", extractor = CabinPriceExtractor, example = "CabinPrice(CA, M1, 200.00)")
    
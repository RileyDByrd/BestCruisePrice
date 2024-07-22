package byrd.riley.bestcruiseprice

import cats.Show
import cats.implicits.toShow

// In every case, the Scala math library consumes far more resources than the Java math library, and there was not a
// requirement to use the Scala math library, so I've imported BigDecimal from the Java math library.
import java.math.BigDecimal

object BestPrice:
  // Here's where the actual logic of finding the best group prices is.
  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] =
    // Create a Map of ratesCodes to rateGroups for which many ratesCodes may map to the same rateGroup.  This is useful
    // because CabinPrices indicate a rateCode but not a rateGroup, and a way is needed to tell which rate group a
    // CabinPrice falls under.  In Scala 2, I would have written `.map(rate => (rate.rateCode, rate.rateGroup))`.  I used
    // `Tuple.fromProductTyped` here because Scala 3 has first-class support for conversion between Tuples and Case Classes.
    // `[String, String]` is not necessary at the end of this line, but IntelliJ has trouble determining the type without it.
    val rateCodeToRateGroup = rates.map(Tuple.fromProductTyped).toMap[String, String]

    // Segregate prices by cabinCode and rateGroup.  This allows all of the prices for a cabin in a rateGroup to be seen.
    val cabinCodeAndRateGroupToCabinPrices = prices.groupBy(price => (price.cabinCode, rateCodeToRateGroup.getOrElse(price.rateCode, throw CabinPriceRateCodeException)))

    // For each cabinCode and rateGroup, find the lowest price (a.k.a. the minimum).
    cabinCodeAndRateGroupToCabinPrices.toSeq.map:
      case ((cabinCode, rateGroup), cabinPrices) =>
        val bestCabinPrice = cabinPrices.minBy(_.price)
        BestGroupPrice(cabinCode = cabinCode, rateCode = bestCabinPrice.rateCode, price = bestCabinPrice.price, rateGroup = rateGroup)
    // Sort by cabinCode and rateCode so that entries are sorted as they appear in the example in the Exercise directions.
    .sortBy(bestGroupPrice => (bestGroupPrice.cabinCode, bestGroupPrice.rateCode))
  end getBestGroupPrices

  private[bestcruiseprice] object CabinPriceRateCodeException extends RuntimeException("An error was encountered while trying to find the rateGroup for the rateCode listed on a CabinPrice.  A Rate must exist for every rateCode on a CabinPrice.")
  
  // Create Show for BestGroupPrice because default String representation does not include the spaces used in the example
  // in the Exercise directions.
  implicit val bestGroupPriceShow: Show[BestGroupPrice] = Show.show(b => s"BestGroupPrice(${b.cabinCode}, ${b.rateCode}, ${b.price}, ${b.rateGroup})")

  // Create Show for Seq[BestGroupPrice] so that entries are spaced according to the example in the Exercise directions.
  implicit val bestGroupPriceSequenceShow: Show[Seq[BestGroupPrice]] = Show.show(seq => seq.map(_.show).mkString("\n"))

  // Classes provided by Exercise directions below.
  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String,
                        rateCode: String,
                        price: BigDecimal)

  case class BestGroupPrice(cabinCode: String,
                            rateCode: String,
                            price: BigDecimal,
                            rateGroup: String)


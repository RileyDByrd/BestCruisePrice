package byrd.riley.bestcruiseprice

import BestPrice.getBestGroupPrices
import InputGathering.{readCabinPriceInput, readRateInput}

import cats.implicits.toShow

@main
def main(): Unit =
  val rates = readRateInput
  val cabinPrices = readCabinPriceInput

  val bestGroupPricesString = getBestGroupPrices(rates.toSeq, cabinPrices.toSeq).show

  println(bestGroupPricesString)

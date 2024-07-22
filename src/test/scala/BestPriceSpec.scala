package byrd.riley.bestcruiseprice

import BestPrice.{BestGroupPrice, CabinPrice, CabinPriceRateCodeException, getBestGroupPrices}
import SpecValues.{exampleCabinPrices, exampleRates}

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.math.BigDecimal

class BestPriceSpec extends AnyWordSpec with Matchers:
  "getBestGroupPrices" must:
    "return the best group prices" when:
      "the example input is given" in:
        getBestGroupPrices(exampleRates.toSeq, exampleCabinPrices.toSeq) mustBe
          Seq(
            BestGroupPrice("CA", "M1", new BigDecimal("200.00"), "Military"),
            BestGroupPrice("CA", "S1", new BigDecimal("225.00"), "Senior"),
            BestGroupPrice("CB", "M1", new BigDecimal("230.00"), "Military"),
            BestGroupPrice("CB", "S1", new BigDecimal("245.00"), "Senior")
          )

    "throw an exception" when:
      "a CabinPrice references a rateCode that doesn't exist" in {
        assertThrows[CabinPriceRateCodeException.type](
          getBestGroupPrices(Seq.empty, Seq(CabinPrice("CA", "S1", new BigDecimal("100.00"))))
        )
      }
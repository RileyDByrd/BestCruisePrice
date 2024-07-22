package byrd.riley.bestcruiseprice

import BestPrice.{CabinPrice, Rate}

import cats.data.NonEmptySeq

import java.math.BigDecimal

object SpecValues {
  val exampleCabinPrices: NonEmptySeq[CabinPrice] = NonEmptySeq.of(
    CabinPrice("CA", "M1", BigDecimal("200.00")),
    CabinPrice("CA", "M2", BigDecimal("250.00")),
    CabinPrice("CA", "S1", BigDecimal("225.00")),
    CabinPrice("CA", "S2", BigDecimal("260.00")),
    CabinPrice("CB", "M1", BigDecimal("230.00")),
    CabinPrice("CB", "M2", BigDecimal("260.00")),
    CabinPrice("CB", "S1", BigDecimal("245.00")),
    CabinPrice("CB", "S2", BigDecimal("270.00"))
  )

  val exampleRates: NonEmptySeq[Rate] = NonEmptySeq.of(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )
}

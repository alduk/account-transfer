package domain

import scalaz._
import Scalaz._
import java.util.Calendar
import scala.concurrent.Future
import domain.model.Account.AccountException

package object service {
  type Valid[F[_]] = {
    type T[A] = EitherT[F, NonEmptyList[AccountException], A]
  }
}

package object model {
  object common {
    type Amount = BigDecimal

    def today = Calendar.getInstance.getTime
  }
}
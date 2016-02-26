package domain
package service

import repository.AccountRepository
import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.Date
import scalaz._
import Scalaz._
import scalaz.Kleisli._
import scalaz.EitherT._

trait AccountService[Account, Amount, Balance] {
  type AccountOperation[F[_], A] = Kleisli[Valid[F]#T, AccountRepository[F], A]

  def open[F[_]](no: String, name: String, openingDate: Option[Date])(implicit F0: Monad[F]): AccountOperation[F, Account]

  def open[F[_]](no: String, name: String, openingDate: Option[Date], amount: Amount)(implicit F0: Monad[F]): AccountOperation[F, Account] = {
    for {
      _ <- open(no, name, openingDate)
      acc1 <- credit(no, amount)
    } yield acc1
  }

  def close[F[_]](no: String, closeDate: Option[Date])(implicit F0: Monad[F]): AccountOperation[F, Account]

  def debit[F[_]](no: String, amount: Amount)(implicit F0: Monad[F]): AccountOperation[F, Account]

  def credit[F[_]](no: String, amount: Amount)(implicit F0: Monad[F]): AccountOperation[F, Account]

  def balance[F[_]](no: String)(implicit F0: Monad[F]): AccountOperation[F, Balance]

  def query[F[_]](no: String)(implicit F0: Monad[F]): AccountOperation[F, Option[Account]]

  def transfer[F[_]](from: String, to: String, amount: Amount)(implicit F0: Monad[F]): AccountOperation[F, (Account, Account)] = for {
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield ((a, b))
}  

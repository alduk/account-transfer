package domain
package repository

import java.util.Date

import domain.model.Account.{AccountException, DoesntExists}

import scalaz._
import Scalaz._
import model.{Account, Balance}

trait AccountRepository[F[_]] {
  type DisE[T] = \/[NonEmptyList[AccountException], T]
  type DisS[T] = \/[NonEmptyList[String], T]
  def query(no: String): F[DisE[Option[Account]]]
  def store(a: Account): F[DisE[Account]]
  def store(a: DisE[Account])(implicit ev:Monad[F]): F[DisE[Account]] = {
    a match {
      case -\/(b) => ev.point(a)
      case \/-(a) => store(a)
    }
  }
  def balance(no: String)(implicit ev:Monad[F]): F[DisE[Balance]] = ev.map(query(no))(_ match {
    case \/-(Some(a)) => a.balance.right
    case \/-(None) => NonEmptyList(DoesntExists(s"No account exists with no $no")).left[Balance]
    case a @ -\/(_) => a
  })
  def query(openedOn: Date): F[DisS[Seq[Account]]]
  def all: F[DisS[Seq[Account]]]
}

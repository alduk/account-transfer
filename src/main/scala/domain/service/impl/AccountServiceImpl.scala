package domain
package service
package impl

import java.util.Date

import domain.model.Account.{ AlreadyExists, DoesntExists }

import scalaz._
import Scalaz._
import \/._
import Kleisli._
import repository.AccountRepository
import model.{ Account, Balance }
import model.common._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class AccountServiceImpl extends AccountService[Account, Amount, Balance] {

  def open[F[_]](no: String, name: String, openingDate: Option[Date])(implicit ev: Monad[F]) : AccountOperation[F, Account] =
    kleisli[Valid[F]#T, AccountRepository[F], Account] { (repo: AccountRepository[F]) =>
      EitherT {
        ev.bind(repo.query(no)) {
          case \/-(Some(a)) => ev.point(NonEmptyList(AlreadyExists(s"Already existing account with no $no")).left[Account])
          case \/-(None) => repo.store(Account.account(no, name, openingDate, None, Balance()))
          case a @ -\/(_) => ev.point(a)
        }
      }
    }

  def close[F[_]](no: String, closeDate: Option[Date])(implicit ev: Monad[F]) = kleisli[Valid[F]#T, AccountRepository[F], Account] { (repo: AccountRepository[F]) =>
    EitherT {
      ev.bind(repo.query(no)) {
        case \/-(None) => ev.point(NonEmptyList(DoesntExists(s"Account $no does not exist")).left[Account])
        case \/-(Some(a)) =>
          val cd = closeDate.getOrElse(today)
          repo.store(Account.close(a, cd))
        case a @ -\/(_) => ev.point(a)
      }
    }
  }

  def debit[F[_]](no: String, amount: Amount)(implicit ev: Monad[F]) = up(no, amount, D)(ev)
  def credit[F[_]](no: String, amount: Amount)(implicit ev: Monad[F]) = up(no, amount, C)(ev)

  private trait DC
  private case object D extends DC
  private case object C extends DC

  private def up[F[_]](no: String, amount: Amount, dc: DC)(implicit ev: Monad[F]): AccountOperation[F, Account] =
    kleisli[Valid[F]#T, AccountRepository[F], Account] { (repo: AccountRepository[F]) =>
      EitherT {
        ev.bind(repo.query(no)) {
          case \/-(None) => ev.point(NonEmptyList(DoesntExists(s"Account $no does not exist")).left[Account])
          case \/-(Some(a)) => dc match {
            case D => repo.store(Account.updateBalance(a, -amount))
            case C => repo.store(Account.updateBalance(a, amount))
          }
          case a @ -\/(_) => ev.point(a)
        }
      }
    }

  def balance[F[_]](no: String)(implicit ev: Monad[F]): AccountOperation[F, Balance] =
    kleisli[Valid[F]#T, AccountRepository[F], Balance] { (repo: AccountRepository[F]) =>
      EitherT {
        repo.balance(no)
      }
    }

  def query[F[_]](no: String)(implicit ev: Monad[F]): AccountOperation[F, Option[Account]] =
    kleisli[Valid[F]#T, AccountRepository[F], Option[Account]] { (repo: AccountRepository[F]) =>
      EitherT {
        repo.query(no)
      }
    }
}

object AccountService extends AccountServiceImpl

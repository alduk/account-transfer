package domain.tests

import java.util.Date
import domain.model.common.Amount
import domain.model.{ Account, Balance }
import domain.repository.impl.AccountRepositoryInMemory
import domain.service.impl.AccountService
import scala.concurrent.ExecutionContext.Implicits._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.prop.{ Checkers, PropertyChecks }
import org.scalatest._
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import domain.repository.impl.AccountRepositoryInMemoryFuture
import scalaz._
import Scalaz._
import domain.repository.AccountRepository
import domain.model.Account.AccountException

/**
 * Created by Admin on 24/2/16.
 */
class AccountServiceSpec extends WordSpec with Matchers with Checkers with PropertyChecks {

  implicit val timeout = 1 seconds

  val arbitraryAccount = for {
    num <- Gen.numStr
    name <- Gen.alphaStr
    amount <- Arbitrary.arbitrary[Amount]
    date <- Arbitrary.arbitrary[Option[Date]]
  } yield Account(num, name, date, None, Balance(amount))

  trait Sync[F[_]] {
    def sync[T](r: => F[T]): T
  }

  object Sync {
    implicit object SyncFuture extends Sync[Future] {
      def sync[T](r: => Future[T]): T = Await.result(r, timeout)
    }

    implicit object Id extends Sync[Id] {
      def sync[T](r: => Id[T]): T = r
    }
  }

  class AccountHelper[F[_]: Monad](db: AccountRepository[F]) {
    type T[A] = \/[NonEmptyList[AccountException], A]

    def open(a: Account) = AccountService.open[F](a.no, a.name, a.openDate).run(db).run
    def openSync(a: Account)(implicit ev: Sync[F]): T[Account] = ev.sync(open(a))
    def openSyncA(a: Account)(implicit ev: Sync[F]): Account = openSync(a).toOption.get

    def query(no: String) = AccountService.query[F](no).run(db).run
    def querySync(no: String)(implicit ev: Sync[F]) = ev.sync(query(no)).toOption.get

    def debit(no: String, amount: Amount) = AccountService.debit[F](no, amount).run(db).run

    def credit(no: String, amount: Amount) = AccountService.credit[F](no, amount).run(db).run
    def creditSync(no: String, amount: Amount)(implicit ev: Sync[F]) = ev.sync(credit(no, amount)).toOption.get

    def transfer(from: String, to: String, amount: Amount) = AccountService.transfer[F](from, to, amount).run(db).run
    def transferSync(from: String, to: String, amount: Amount)(implicit ev: Sync[F]) = ev.sync(transfer(from, to, amount))
  }

  val positiveAmount = Gen.choose(0, 1000000.0).map(BigDecimal.apply)
  val accountValidNo = arbitraryAccount.filter(_.no.size >= 5)
  val accountNormal = accountValidNo.filter(_.no.size < 10)

  def testWithRepo[F[_]](account: AccountHelper[F], name: String)(implicit ev: Sync[F]) = {
    s"Account service with $name" must {
      "open account" in {
        forAll(arbitraryAccount) { (a: Account) =>
          val r = account.openSync(a)
          r.isLeft should be(if (a.no.size < 5) true else false)
        }
      }
    }
    it should {
      "be able to credit account" in {
        forAll(Gen.nonEmptyListOf(positiveAmount)) { (amounts: List[Amount]) =>
          val sample = accountValidNo.sample
          whenever(sample.isDefined) {
            val acc = account.openSyncA(sample.get)
            val updates = amounts.map(a => account.creditSync(acc.no, a))
            val updatedAcc = account.querySync(acc.no).get
            (updatedAcc.balance.amount - acc.balance.amount) should be(amounts.reduce(_ + _))
          }
        }
      }
    }

    it should {
      "be able to deposit account" in {
        forAll(accountValidNo, accountValidNo) { (acc1, acc2) =>
          val g1 = account.creditSync(account.openSyncA(acc1).no, positiveAmount.sample.get)
          val g2 = account.creditSync(account.openSyncA(acc2).no, positiveAmount.sample.get)
          val amountOpt = Gen.choose(0, g1.balance.amount.doubleValue).sample
          whenever(amountOpt.isDefined) {
            val amount = amountOpt.get
            val (u1, u2) = account.transferSync(g1.no, g2.no, amount).toOption.get
            u1.balance.amount should be(g1.balance.amount - amount)
            u2.balance.amount should be(g2.balance.amount + amount)
          }
        }
      }
    }

    it should {
      "be able to transfer between accounts" in {
        forAll(accountValidNo, accountValidNo) { (acc1, acc2) =>
          val g1 = account.creditSync(account.openSyncA(acc1).no, positiveAmount.sample.get)
          val g2 = account.creditSync(account.openSyncA(acc2).no, positiveAmount.sample.get)
          val amountOpt = Gen.choose(g1.balance.amount.doubleValue + 1, Double.MaxValue).sample
          whenever(amountOpt.isDefined) {
            val amount = amountOpt.get
            //println(s"Transferring $amount from $g1   to $g2 ")
            val r = account.transferSync(g1.no, g2.no, amount)
            r.isLeft should be(true)
          }
        }
      }
    }
  }
  
    //val account = new AccountHelper(AccountRepositoryInMemoryFuture)
  val helperInMemory = new AccountHelper(AccountRepositoryInMemory)
  val helperInMemoryFuture = new AccountHelper(AccountRepositoryInMemoryFuture)
  
  testWithRepo(helperInMemory, "(in memory ID)")
  testWithRepo(helperInMemoryFuture, "(in memory Future)")
}

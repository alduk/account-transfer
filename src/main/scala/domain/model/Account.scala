package domain.model

import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }
import scalaz._
import Scalaz._

/**
 * Created by Admin on 21/2/16.
 */
import common._

case class Balance(amount: Amount = 0)

case class Account(no: String, name: String, openDate: Option[Date], closeDate: Option[Date], balance: Balance) {
  def isOpen = openDate.isDefined && !closeDate.isDefined
  def isClosed = !isOpen
}

object Account {

  abstract class AccountException(message: String) extends Exception(message)

  case class InvalidAccountNo(message: String) extends AccountException(message)

  case class InvalidOpenCloseDate(message: String) extends AccountException(message)

  case class AlreadyClosed(message: String) extends AccountException(message)

  case class AlreadyExists(message: String) extends AccountException(message)

  case class DoesntExists(message: String) extends AccountException(message)

  case class InsufficientBalance(message: String) extends AccountException(message)

  private def validateAccountNo(no: String): ValidationNel[AccountException, String] =
    if (no.isEmpty || no.size < 5)
      InvalidAccountNo(s"Account No has to be at least 5 characters long: found $no").failureNel[String]
    else no.successNel[AccountException]

  private def validateOpenCloseDate(od: Date, cd: Option[Date]): ValidationNel[AccountException, (Option[Date], Option[Date])] = cd.map { c =>
    if (c before od)
      InvalidOpenCloseDate(s"Close date [$c] cannot be earlier than open date [$od]").failureNel[(Option[Date], Option[Date])]
    else (od.some, cd).successNel[AccountException]
  }.getOrElse { (od.some, cd).successNel[AccountException] }

  def account(no: String, name: String, openDate: Option[Date], closeDate: Option[Date], balance: Balance): \/[NonEmptyList[AccountException], Account] = {
    (validateAccountNo(no) |@| validateOpenCloseDate(openDate.getOrElse(today), closeDate)) { (n, d) =>
      Account(n, name, d._1, d._2, balance)
    }.disjunction
  }

  private def validateAccountAlreadyClosed(a: Account): ValidationNel[AccountException, Account] = {
    if (a.isClosed) AlreadyClosed(s"Account ${a.no} is already closed").failureNel[Account]
    else a.successNel[AccountException]
  }

  private def validateCloseDate(a: Account, cd: Date): ValidationNel[AccountException, Date] = {
    if (cd before a.openDate.get) InvalidOpenCloseDate(s"Close date [$cd] cannot be earlier than open date [${a.openDate.get}]").failureNel[Date]
    else cd.successNel[AccountException]
  }

  def close(a: Account, closeDate: Date): \/[NonEmptyList[AccountException], Account] = {
    (validateAccountAlreadyClosed(a) |@| validateCloseDate(a, closeDate)) { (acc, d) =>
      acc.copy(closeDate = Some(closeDate))
    }.disjunction
  }

  private def checkBalance(a: Account, amount: Amount) = {
    if (amount < 0 && a.balance.amount < -amount) InsufficientBalance(s"Insufficient amount in ${a.no} to debit").failureNel[Account]
    else a.successNel[AccountException]
  }

  def updateBalance(a: Account, amount: Amount): \/[NonEmptyList[AccountException], Account] = {
    (validateAccountAlreadyClosed(a) |@| checkBalance(a, amount)) { (_, _) =>
      a.copy(balance = Balance(a.balance.amount + amount))
    }.disjunction
  }

}

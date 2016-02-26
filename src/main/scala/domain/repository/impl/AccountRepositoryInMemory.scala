package domain
package repository
package impl

import java.util.Date
import domain.model.Account.AccountException
import scala.collection.mutable.{Map => MMap}
import scalaz._
import Scalaz._
import model._
import scala.concurrent.Future

trait AccountRepositoryInMemory extends AccountRepository[Id] {
  lazy val repo = MMap.empty[String, Account]

  def query(no: String): \/[NonEmptyList[AccountException], Option[Account]] = repo.get(no).right
  def store(a: Account): \/[NonEmptyList[AccountException], Account] = {
    val r = repo += ((a.no, a))
    a.right
  }
  def query(openedOn: Date): \/[NonEmptyList[String], Seq[Account]] = repo.values.filter(_.openDate == openedOn).toSeq.right
  override def all: \/[NonEmptyList[String], Seq[Account]] = repo.values.toSeq.right

}

trait AccountRepositoryInMemoryFuture extends AccountRepository[Future] {
  lazy val repo = MMap.empty[String, Account]

  def query(no: String): Future[DisE[Option[Account]]] = Future.successful(repo.get(no).right)
  def store(a: Account): Future[DisE[Account]] = {
    val r = repo += ((a.no, a))
    Future.successful(a.right)
  }
  def query(openedOn: Date): Future[DisS[Seq[Account]]] = Future.successful(repo.values.filter(_.openDate == openedOn).toSeq.right)
  def all: Future[DisS[Seq[Account]]] = Future.successful(repo.values.toSeq.right)

}

object AccountRepositoryInMemory extends AccountRepositoryInMemory
object AccountRepositoryInMemoryFuture extends AccountRepositoryInMemoryFuture

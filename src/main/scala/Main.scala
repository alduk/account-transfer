import java.util.Date

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.typesafe.config.{Config, ConfigFactory}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink

import scala.concurrent.ExecutionContext
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.ParameterDirectives
import domain.model.{Account, Balance}
import domain.model.common.Amount
import domain.repository.AccountRepository
import domain.repository.impl.AccountRepositoryInMemory
import domain.service.AccountService
import domain.service.impl.AccountService
import domain.json.AccountJsonSupport._

trait Core {
  implicit val system: ActorSystem = ActorSystem("account-transfer")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = system.dispatcher
}

object BootAkkaHttp extends App {
  val http = new AkkaHttp with Core {
    override def config: Config = ConfigFactory.load()
    override val accountService = AccountService
    override val accountRepository = AccountRepositoryInMemory
  }

  http.start()
}

trait AkkaHttp {
  this: Core =>

  def config: Config

  def accountService: AccountService[Account, Amount, Balance]
  def accountRepository: AccountRepository[scalaz.Scalaz.Id]
  def start() = {
    val route: Route = {
      path("dictionaries" / Segment / "suggestions") { dictionaryId =>
        get {
          parameters("ngr") { ngr =>
            complete("response")
          }
        }
      } ~
        path("accounts" / "open") {
          (get & parameters('number.as[String], 'name.as[String], 'amount.as[Double] ? )) { (number, name, amount) =>
            complete {
              val openAccount = accountService.open(number, name, Some(new Date), BigDecimal(amount.getOrElse(0.0)))
              openAccount.run(accountRepository).run
            }
          }
        } ~
        path("accounts" / "list") {
          complete {
            accountRepository.all
          }
        } ~
        path("accounts" / "debit") {
          (get & parameters('number.as[String], 'amount.as[Double])) { (number, amount) =>
            complete {
              accountService.debit(number, amount).run(accountRepository).run
            }
          }
        } ~
        path("accounts" / "credit") {
          (get & parameters('number.as[String], 'amount.as[Double])) { (number, amount) =>
            complete {
              accountService.credit(number, amount).run(accountRepository).run
            }
          }
        } ~
        path("accounts" / "transfer") {
          (get & parameters('from.as[String], 'to.as[String], 'amount.as[Double])) { (from, to, amount) =>
            complete {
              accountService.transfer(from, to, amount).run(accountRepository).run
            }
          }
        }
    }

    Http().bind(
      interface = config.getString("frontend.interface"),
      port = config.getInt("frontend.port")).to(Sink.foreach { conn =>
        conn.flow.join(route).run()
      }).run()
  }
}

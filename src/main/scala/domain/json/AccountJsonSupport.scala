package domain.json

import java.util.Date

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import domain.model.Account.{AccountException, InvalidAccountNo}
import domain.model.{Account, Balance}
import spray.json._

import scalaz.{-\/, NonEmptyList, \/, \/-}

/**
  * Created by Admin on 24/2/16.
  */
object AccountJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {

  implicit val DateFormat = new RootJsonFormat[Date] {
    lazy val format = new java.text.SimpleDateFormat()
    def read(json: JsValue): Date = new Date(new java.lang.Long(json.compactPrint))
    def write(date: Date) = JsString(date.getTime.toString)
  }

  implicit def DisjunctionFormat[A:JsonFormat,B:JsonFormat]: RootJsonFormat[\/[A,B]] = new RootJsonFormat[\/[A,B]] {
    def read(json: JsValue): \/[A,B] = \/-(implicitly[JsonFormat[B]].read(json))
    def write(v: \/[A,B]) = v match {
      case -\/(a) => implicitly[JsonFormat[A]].write(a)
      case \/-(b) => implicitly[JsonFormat[B]].write(b)
    }
  }

  implicit def viaNel[T :JsonFormat]: RootJsonFormat[NonEmptyList[T]] = new RootJsonFormat[NonEmptyList[T]] {
    def write(nel: NonEmptyList[T]) = JsArray(nel.map(_.toJson).list : _ *)
    def read(value: JsValue) = value match {
      case JsArray(elements) => {
        val els = elements.map(_.convertTo[T])
        NonEmptyList(els.head, els.tail : _*)
      }
      case x => deserializationError("Expected Collection as JsArray, but got " + x)
    }
  }

  implicit val AccountExceptionFormats = new RootJsonFormat[AccountException] {
    def write(ex: AccountException) = JsString(ex.getMessage)
    def read(value: JsValue) = InvalidAccountNo(value.compactPrint)
  }

  implicit val BalanceFormats = jsonFormat1(Balance.apply)
  implicit val AccountFormats = jsonFormat5(Account.apply)
}

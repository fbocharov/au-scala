package ru.spbau.bocharov.currency

import play.api.libs.json.{Json, JsNumber, JsObject, JsValue}

import scala.io.Source

trait ExchangeRateProvider {
  def get(from: Currency, to: Currency): Double
}

object ExchangeRate {
  def apply(date: Option[Date]) = new CurrencyLayerExchangeRateProvider(date)
}

class CurrencyLayerExchangeRateProvider(date: Option[Date]) extends ExchangeRateProvider {
  private val API_HTTP = "http://apilayer.net/api"
  private val API_KEY = "ab54c890d53743feb40b8cc26674c72a"
  private val COMMON_PARAMS = s"access_key=$API_KEY&currencies=%s"

  private val REQUEST_PATTERN_CURRENT = s"$API_HTTP/live?$COMMON_PARAMS"
  private val REQUEST_PATTERN_ON_DATE = s"$API_HTTP/historical?$COMMON_PARAMS&date=%s"

  override def get(from: Currency, to: Currency): Double = {
    val toUSDRates = requestToUSDRates(createRequest(date, List(from, to)))
    toUSDRates(to) / toUSDRates(from)
  }

  private def createRequest(date: Option[Date], currencies: List[Currency]): String = {
    date match {
      case None => REQUEST_PATTERN_CURRENT.format(currencies.map(_.code).mkString(","))
      case Some(dd) => REQUEST_PATTERN_ON_DATE.format(currencies.map(_.code).mkString(","), dd.toString)
    }
  }

  private def requestToUSDRates(req: String): Map[Currency, Double] = {
    def die(response: JsValue) =
      throw new RuntimeException(s"failed to request rate: $response")

    val json = Json.parse(Source.fromURL(req).mkString)
    json match {
      case JsObject(fields) => fields.get("quotes") match {
        case Some(JsObject(result)) => result.map {
          case (name, JsNumber(value)) =>
            (Currency.fromString(name.substring(3)), value.toDouble)
          case _ => die(json)
        }.toMap
        case _ => die(json)
      }
      case _ => die(json)
    }
  }
}
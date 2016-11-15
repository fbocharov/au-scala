package ru.spbau.bocharov.currency

trait Currency {
  val code: String
}

object Currency {
  def fromString(str: String): Currency = {
    str match {
      case "USD" => USD
      case "EUR" => EUR
      case "RUB" => RUB
      case _ => throw new RuntimeException(s"unknown currency: $str")
    }
  }
}

object USD extends Currency {
  override val code: String = "USD"
}

object EUR extends Currency {
  override val code: String = "EUR"
}

object RUB extends Currency {
  override val code: String = "RUB"
}
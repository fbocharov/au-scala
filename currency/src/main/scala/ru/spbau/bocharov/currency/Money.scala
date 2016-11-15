package ru.spbau.bocharov.currency

class Money(count: Double, fromCurrency: Currency) {
  def to(toCurrency: Currency) = new Conversion(count, fromCurrency, toCurrency)
}

object MoneyConversions {
  implicit class ToMoney(count: Double) {
    def apply(currency: Currency) = new Money(count, currency)
  }
}

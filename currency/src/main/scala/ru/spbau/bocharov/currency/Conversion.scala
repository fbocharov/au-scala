package ru.spbau.bocharov.currency

class Conversion(count: Double, from: Currency, to: Currency) {

  private var date: Option[Date] = None
  private var result: Option[Double] = None

  def on(d: Date) = {
    date = Some(d)
    // need to clear result on new date
    result = None
    this
  }

  def convert: Double = {
    if (result.isEmpty) {
      val rate: Double = if (from == to) 1.0 else ExchangeRate(date).get(from, to)
      result = Option(rate * count)
    }
    result.get
  }
}

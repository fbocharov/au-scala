package ru.spbau.bocharov.currency

class Date(day: Int, month: Int, year: Int) {
  override def toString: String = {
    val d = if (day < 10) s"0$day" else s"$day"
    val m = if (month < 10) s"0$month" else s"$month"
    s"$year-$m-$d"
  }
}

object DateConversions {
  class DayMonth(day: Int, month: Int) {
    def \(year: Int) = new Date(day, month, year)
  }

  implicit class Day(day: Int) {
    def \(month: Int) = new DayMonth(day, month)
  }
}
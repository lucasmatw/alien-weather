package model

/**
  * Wraps a list of WeatherForecast for further analysis.
  *
  * @param weatherForecast
  */
case class WeatherBatchResult(weatherForecast :List[WeatherForecast]) {

  def day(day :Int) :Option[WeatherForecast] = {
    weatherForecast.find(_.solarSystem.age == day)
  }

  def rainyDays() :Int = {
    weatherForecast.count(rainyDay)
  }

  def droughDays() :Int = {
    weatherForecast.collect{ case f :Drought => f}.length
  }

  def optimalDays() :Int = {
      weatherForecast.collect{ case f :Optimal => f}.length
  }

  def rainyDay(forecast :WeatherForecast) :Boolean = { forecast match {
      case Rain(_, _) => true
      case _ => false
    }
  }

  /**
    * Returns first day with maximum rain density.
    *
    * @return
    */
  def mostRainyDay() :Option[WeatherForecast] = {
    weatherForecast.filter(rainyDay).sortWith((x, y) => x.density.get > y.density.get).headOption
  }
}

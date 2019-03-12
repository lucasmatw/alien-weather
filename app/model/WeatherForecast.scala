package model

class WeatherForecast(val solarSystem: SolarSystem, val description :String, val density :Option[Int])

case class Normal(system: SolarSystem) extends WeatherForecast(system,"Normal", None)
case class Drought(system: SolarSystem) extends WeatherForecast(system,"Sequia", None)
case class Optimal(system: SolarSystem) extends WeatherForecast(system,"Optima presion y temperatura", None)
case class Rain(system: SolarSystem, rainDensity :Option[Int]) extends WeatherForecast(system,"Lluvia", rainDensity)




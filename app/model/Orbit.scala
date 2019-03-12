package model

trait Orbit {
  def move(solarPosition: SolarPosition, days :Int) :SolarPosition
}

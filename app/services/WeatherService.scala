package services

import model._

/**
  * Forecasts solar system weather.
  *
  * NOTES:
  * For now it only works for tri-planetary system only.
  *
  */
class WeatherService {

  /**
    * Returns solar system weather for its current state.
    *
    * @param solarSystem
    * @return
    */
  def forecast(solarSystem: SolarSystem) :WeatherForecast = {
    if(this.isOptimal(solarSystem)) Optimal(solarSystem)
    else if(this.isRain(solarSystem)){
      val density :Option[Int] = Some(this.planetsTriangleArea(solarSystem))
      Rain(solarSystem, density)
    } else if(this.isDrought(solarSystem)) Drought(solarSystem)
    else Normal(solarSystem)
  }

  /**
    * Returns true if weather is "optimal".
    *
    * Weather is optimal if planets are aligned but differs in positional degrees.
    *
    * @param solarSystem
    * @return
    */
  def isOptimal(solarSystem: SolarSystem): Boolean = {

    val first = solarSystem.planet(0)
    val second = solarSystem.planet(1)
    val third = solarSystem.planet(2)

    this.aligned(solarSystem) && (first.position.clockDegrees != second.position.clockDegrees) &&
      (second.position.clockDegrees != third.position.clockDegrees)

  }

  /**
    * Returns true if planets are aligned.
    *
    * Planets are aligned if planetary triangle area is 0.
    *
    * @param solarSystem
    * @return
    */
  def aligned(solarSystem: SolarSystem) :Boolean = {
    this.planetsTriangleArea(solarSystem) == 0
  }

  /**
    * Returns planetary triangle area.
    *
    * @param solarSystem
    * @return
    */
  def planetsTriangleArea(solarSystem: SolarSystem): Int = {

    val a = solarSystem.planet(0).position
    val b = solarSystem.planet(1).position
    val c = solarSystem.planet(2).position

    val ab = (b.x - a.x, b.y - a.y)

    val nab = (ab._2, - ab._1)

    val ac = (c.x - a.x, c.y - a.y)

    val res = (0.5 * Math.abs(nab._1 * ac._1 + nab._2 * ac._2)).toInt

    res
  }

  /**
    * Returns true if it's a rainy day in given SolarSystem.
    *
    * Checks planets are not aligned and sun is inside the planetary triangle.
    *
    * @param solarSystem
    * @return
    */
  def isRain(solarSystem: SolarSystem): Boolean = {

    val first = solarSystem.planet(0)
    val second = solarSystem.planet(1)
    val third = solarSystem.planet(2)

    !this.aligned(solarSystem) && centerInTriangle(first.position, second.position, third.position)
  }

  def sign(sp1 :SolarPosition, sp2 :SolarPosition, sp3 :SolarPosition) :Int = {
     (sp1.x - sp3.x) * (sp2.y - sp3.y) - (sp2.x - sp3.x) * (sp1.y - sp3.y)
  }

  /**
    * Returns true if planetary triangle contains the sun (point (0, 0)).
    *
    * @param sp1
    * @param sp2
    * @param sp3
    * @return
    */
  def centerInTriangle(sp1 :SolarPosition, sp2 :SolarPosition, sp3 :SolarPosition) :Boolean = {
    val pt = SolarPosition(0, 0)

    val s1 = sign(pt, sp1, sp2)
    val s2 = sign(pt, sp2, sp3)
    val s3 = sign(pt, sp3, sp1)

    val hasNeg = (s1 < 0) || (s2 < 0) || (s3 < 0)
    val hasPos = (s1 > 0) || (s2 > 0) || (s3 > 0)

    !(hasNeg && hasPos)
  }

  def simplifyAngle(degrees :Int) :Int = {
    if(degrees > 360){degrees - 360} else if(degrees < 0) degrees + 360 else degrees
  }

  /**
    * Dado un SolarSystem determina si hay sequia.
    *
    * Chequea que los planetas esten alineados entre si, y que el centro este en el medio.
    *
    * @param solarSystem
    * @return
    */
  def isDrought(solarSystem: SolarSystem): Boolean = {

    val first = solarSystem.planet(0).position
    val second = solarSystem.planet(1).position
    val third = solarSystem.planet(2).position

    this.aligned(solarSystem) && this.alignedAngle(first, second) && this.alignedAngle(second, third)
  }

  /**
    * Determina si dos SolarPosition estan alineadas segun su angulo.
    *
    * @param posX
    * @param posY
    * @return
    */
  def alignedAngle(posX: SolarPosition, posY: SolarPosition) :Boolean = {
    posX.clockDegrees == posY.clockDegrees ||
      (posX.clockDegrees + 180) == posY.clockDegrees ||
      (posX.clockDegrees - 180) == posY.clockDegrees
  }

  /**
    * Builds a WeatherBatchResult with required galaxy.
    *
    * @param days
    * @return
    */
  def distantGalaxyForecast(days :Int) : WeatherBatchResult = {

    val f = Planet(SolarPosition(0, 500), CircularOrbit(1, clockwise = true), "Ferengi")
    val b = Planet(SolarPosition(0, 2000), CircularOrbit(3, clockwise = true), "Betasoide")
    val v = Planet(SolarPosition(0, 1000), CircularOrbit(5, clockwise = false), "Vulcano")

    val solarSystem = SolarSystem(List(f, b, v))

    val forecasts = List.range(0, days)
      .map(solarSystem.simulate)
      .map(this.forecast)

    WeatherBatchResult(forecasts)
  }
}

/**
  * Simulates persistence in app.
  */
object WeatherService {
  var globalForecast = new WeatherBatchResult(List())
}

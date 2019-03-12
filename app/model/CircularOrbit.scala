package model


case class CircularOrbit(speed :Int, clockwise :Boolean) extends Orbit {

  /**
    * "Moves" a SolarPosition.
    *
    * Given a SolarPosition, builds a new SolarPosition using speed and direction.
    *
    * @param solarPosition
    * @param days
    * @return
    */
  override def move(solarPosition: SolarPosition, days :Int): SolarPosition = {

    val direction = if(clockwise) 1 else -1
    val toMove = days*speed*direction

    val currentDegree = solarPosition.clockDegrees

    val newDegree = currentDegree + toMove

    val cos = Math.cos(Math.toRadians(newDegree))
    val sin = Math.sin(Math.toRadians(newDegree))

    val radius = Math.sqrt(solarPosition.x * solarPosition.x + solarPosition.y * solarPosition.y)

    val unclockX = cos * radius
    val unclockY = sin * radius

    SolarPosition(round(unclockY), round(unclockX))
  }

  def round(value :Double) :Int = {
    "%.0f".format(value).toInt
  }
}

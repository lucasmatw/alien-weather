package model

/**
  * Position with respect to the sun (x = 0, y = 0).
  *
  * @param x
  * @param y
  */
case class SolarPosition(x :Int, y :Int) {

  /**
    * Returns degrees as seen as a clock.
    *
    * 12:00 = 0º
    * 03:00 = 90º
    * 06:00 = 180º
    *
    * @return
    *         integer representing degrees between 0 and 360
    */
  def clockDegrees :Int = {
    //swap to "unclock"
    val degrees = Math.toDegrees(Math.atan2(x, y)).toInt
    if(degrees < 0) 360 + degrees else degrees
  }

  override def toString: String = s"($x, $y, deg=$clockDegrees)"
}

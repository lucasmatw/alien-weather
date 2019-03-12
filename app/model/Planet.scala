package model

case class Planet(position: SolarPosition, orbit: Orbit, name :String = "none") {

  /**
    * "Moves" using its orbit. Returns a new Planet.
    *
    * @param days
    * @return
    */
  def move(days :Int) :Planet = {
    Planet(orbit.move(position, days), orbit, name)
  }

  override def toString: String = s"P($name, pos=$position)"
}

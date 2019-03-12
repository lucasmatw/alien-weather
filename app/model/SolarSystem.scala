package model

case class SolarSystem(planets :List[Planet], age :Int = 0) {

  /**
    * Returns a new SolarSystem representing it state at given day.
    * Moves all planets using given day and increments system's age.
    *
    * @param days
    * @return
    */
  def simulate(days :Int) :SolarSystem = {
    SolarSystem(planets.map(_.move(days)), age + days)
  }

  def planet(index :Int) :Planet = planets(index)

  override def toString: String = planets.toString()
}

package services

import model.{CircularOrbit, Planet, SolarPosition, SolarSystem}
import org.scalatestplus.play.PlaySpec

class WeatherServiceSpec extends PlaySpec {

  "WeatherService isOptimal" should {
    "return true when positions are" in {
      val posX = SolarPosition(0, 100)
      val posY = SolarPosition(50, 50)
      val posZ = SolarPosition(100, 0)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val optimal = weatherService.isOptimal(solarSystem)
      val drought = weatherService.isDrought(solarSystem)
      val rain = weatherService.planetsTriangleArea(solarSystem)


      optimal mustBe true
      drought mustBe false
      rain mustBe 0
    }

    "return true when a negative position" in {
      val posX = SolarPosition(-10, 50)
      val posY = SolarPosition(0, 60)
      val posZ = SolarPosition(10, 70)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val optimal = weatherService.isOptimal(solarSystem)
      val drought = weatherService.isDrought(solarSystem)
      val rain = weatherService.planetsTriangleArea(solarSystem)

      optimal mustBe true
      drought mustBe false
      rain mustBe 0
    }


    "return false when" in {
      val posX = SolarPosition(500, 0)
      val posY = SolarPosition(-2000, 0)
      val posZ = SolarPosition(-1000, 0)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val optimal = weatherService.isOptimal(solarSystem)
      val drought = weatherService.isDrought(solarSystem)


      optimal mustBe false
      drought mustBe true
    }
  }

  "WeatherService drought" should {
    "return true when" in {
      val posX = SolarPosition(100, 100)
      val posY = SolarPosition(200, 200)
      val posZ = SolarPosition(500, 500)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val drought = weatherService.isDrought(solarSystem)
      val optimal = weatherService.isOptimal(solarSystem)

      optimal mustBe false
      drought mustBe true
    }

    "return true when 0 on X" in {
      val posX = SolarPosition(0, -500)
      val posY = SolarPosition(0, -2000)
      val posZ = SolarPosition(0, -1000)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val drought = weatherService.isDrought(solarSystem)
      val optimal = weatherService.isOptimal(solarSystem)

      optimal mustBe false
      drought mustBe true
    }

    "return true when 0 on Y" in {
      val posX = SolarPosition(100, 0)
      val posY = SolarPosition(200, 0)
      val posZ = SolarPosition(5000, 0)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val drought = weatherService.isDrought(solarSystem)
      val optimal = weatherService.isOptimal(solarSystem)

      drought mustBe true
      optimal mustBe false
    }


    "return false when" in {
      val posX = SolarPosition(-100, 100)
      val posY = SolarPosition(-50, 150)
      val posZ = SolarPosition(-25, 175)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val drought = weatherService.isDrought(solarSystem)

      drought mustBe false
    }
  }

  "WeatherService is rain" should {
    "return true when" in {

      val posX = SolarPosition(-1000, 700)
      val posY = SolarPosition(800, 200)
      val posZ = SolarPosition(10, -800)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val rain = weatherService.isRain(solarSystem)

      rain mustBe true
    }

    "return true when two planets in 4th quadrant" in {

      val posX = SolarPosition(-70, 50)
      val posY = SolarPosition(-20, 80)
      val posZ = SolarPosition(30, -30)

      val orbit = CircularOrbit(0, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystemOne = SolarSystem(List(planetX, planetY, planetZ))
      val solarSystemTwo = SolarSystem(List(planetZ, planetY, planetX))
      val solarSystemThree = SolarSystem(List(planetX, planetZ, planetY))

      val weatherService = new WeatherService

      val rainOne = weatherService.isRain(solarSystemOne)
      val rainTwo = weatherService.isRain(solarSystemTwo)
      val rainThree = weatherService.isRain(solarSystemThree)

      rainOne mustBe true
      rainTwo mustBe true
      rainThree mustBe true
    }

    "return false when" in {

      val posX = SolarPosition(-1000, 700)
      val posY = SolarPosition(800, 200)
      val posZ = SolarPosition(10, 100)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val rain = weatherService.isRain(solarSystem)

      rain mustBe false
    }
  }

  "WeatherService rain density" should {
    "return 200" in {
      val posX = SolarPosition(0, 200)
      val posY = SolarPosition(0, 0)
      val posZ = SolarPosition(200, 0)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val rain = weatherService.planetsTriangleArea(solarSystem)

      rain mustBe 20000
    }

    "return 8" in {
      val posX = SolarPosition(0, 4)
      val posY = SolarPosition(0, 0)
      val posZ = SolarPosition(4, 0)

      val orbit = CircularOrbit(90, true)

      val planetX = Planet(posX, orbit)
      val planetY = Planet(posY, orbit)
      val planetZ = Planet(posZ, orbit)

      val solarSystem = SolarSystem(List(planetX, planetY, planetZ))

      val weatherService = new WeatherService

      val rain = weatherService.planetsTriangleArea(solarSystem)

      rain mustBe 8
    }
  }

  "WeatherService batch forecast" should {

    "print first 90 days" in {
      val weatherService = new WeatherService

      val result = weatherService.distantGalaxyForecast(90)

      result.weatherForecast.foreach(wf => println(s"SYSTEM= " + wf))
    }

    "answer" in {
      val weatherService = new WeatherService

      val result = weatherService.distantGalaxyForecast(3650)

      println("LLUVIAS = " + result.rainyDays())

      println("SEQUIAS = " + result.droughDays())

      println("OPTIMOS = " + result.optimalDays())

      println("DIA MAS LLUVIOSO = " + result.mostRainyDay().get.solarSystem.age)
      println("MAXIMA LLUVIA = " + result.mostRainyDay().get.density)

    }
  }
}

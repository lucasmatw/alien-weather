package model

import org.scalatestplus.play.PlaySpec

class CircularOrbitSpec extends PlaySpec {

  "CircularOrbit move" should {

    "change position correctly 90" in {
      val pos = SolarPosition(100, 0)

      val orbit = CircularOrbit(90, true)

      val newPos = orbit.move(pos, 1)

      newPos.x mustBe 0
      newPos.y mustBe -100
    }

    "change position correctly 180" in {
      val pos = SolarPosition(0, 100)

      val orbit = CircularOrbit(180, true)

      val newPos = orbit.move(pos, 1)

      newPos.x mustBe 0
      newPos.y mustBe -100
    }

    "change position correctly at 0 after moves" in {
      val pos = SolarPosition(0, 100)

      val orbit = CircularOrbit(1, true)

      val newPos = orbit.move(pos, 360)

      newPos.x mustBe 0
      newPos.y mustBe 100
    }

    "change position anti clockwise 360" in {
      val pos = SolarPosition(0, 100)

      val orbit = CircularOrbit(1, false)

      val newPos = orbit.move(pos, 360)

      newPos.x mustBe 0
      newPos.y mustBe 100
    }

    "change position anti clockwise 90" in {
      val pos = SolarPosition(0, 100)

      val orbit = CircularOrbit(45, false)

      val newPos = orbit.move(pos, 2)

      newPos.x mustBe -100
      newPos.y mustBe 0
    }

    "change position anti clockwise 45" in {
      val pos = SolarPosition(0, 1000)

      val orbit = CircularOrbit(9, false)

      val newPos = orbit.move(pos, 10)

      newPos.x mustBe -1000
      newPos.y mustBe 0
    }
  }

}

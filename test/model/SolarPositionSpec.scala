package model

import org.scalatestplus.play._

class SolarPositionSpec extends PlaySpec {

  "SolarPostion clockDegree" should {

    "return degrees as clock position" in {
      val pos = SolarPosition(1, -1)
      pos.clockDegrees mustBe 135
    }

    "return 90 degrees" in {
      val pos = SolarPosition(1, 0)
      pos.clockDegrees mustBe 90
    }

    "return 0 degrees" in {
      val pos = SolarPosition(0, 1)
      pos.clockDegrees mustBe 0
    }

    "return 0 degrees scaled" in {
      val pos = SolarPosition(0, 200)
      pos.clockDegrees mustBe 0
    }

    "return 180 degrees" in {
      val pos = SolarPosition(0, -1)
      pos.clockDegrees mustBe 180
    }

    "return 270 degrees" in {
      val pos = SolarPosition(-1, 0)
      pos.clockDegrees mustBe 270
    }

    "return 225 degrees" in {
      val pos = SolarPosition(-1, -1)
      pos.clockDegrees mustBe 225
    }
  }
}

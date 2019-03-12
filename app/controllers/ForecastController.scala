package controllers

import javax.inject._
import model.ForecastDay
import play.api._
import play.api.mvc._
import services.WeatherService
import play.api.libs.json._

@Singleton
class ForecastController @Inject()(cc: ControllerComponents, weatherService: WeatherService) extends AbstractController(cc) {

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok("Only working route is /clima?dia=xxx")
  }

  def clima(day :String) = Action { implicit request: Request[AnyContent] =>

    implicit lazy val jsonFormat: Format[ForecastDay] = Json.format[ForecastDay]

    val dayInt = day.toInt

    if(WeatherService.globalForecast.weatherForecast.isEmpty){
      WeatherService.globalForecast = weatherService.distantGalaxyForecast(3650)
    }

    val fc = WeatherService.globalForecast.day(dayInt)

    fc.map(wfc => {
      Ok(Json.toJson(ForecastDay(dayInt , wfc.description)))
    }).getOrElse(BadRequest(Json.obj("error" -> s"day $day not found")))
  }
}

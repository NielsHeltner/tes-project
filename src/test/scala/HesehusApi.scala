import java.io.FileInputStream
import scalaj.http.Http
import play.api.libs.json.{JsObject, Json}
import scala.io.Source

class HesehusApi {
  val config: JsObject = Json.parse(getClass.getResourceAsStream("config.json")).as[JsObject]
  val host = config.value("host").as[String]
  val port = config.value("port").as[String]

  val apiKey: String = {
    val request = Http("http://" + host + ":" + port + "/api/v1/Auth").param("apiKey", "t1").postData("")
    val response = request.asString
    val json = Json.parse(response.body).as[JsObject]
    json.value("accessToken").as[String]
  }
  //println(apiKey)
  //println(getAlias.mkString("[", ",", "]"))

  def getAmount: String = {
    "do http get"
  }

  def getAlias: Array[String] = {
    val request = Http("http://" + host + ":" + port + "/api/productsearch/v1/Alias").header("Authorization", s"Bearer $apiKey")
    val response = request.asString
    Json.parse(response.body).as[Array[String]]
  }

}

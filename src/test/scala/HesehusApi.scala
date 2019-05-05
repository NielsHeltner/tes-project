import play.api.libs.json.{JsObject, Json}
import scalaj.http.Http

class HesehusApi {

  val apiKey: String = {
    val request = Http("http://localhost:5000/api/v1/Auth").param("apiKey", "t1").postData("")
    val response = request.asString
    val json = Json.parse(response.body).as[JsObject]
    json.value("accessToken").as[String]
  }
  //println(apiKey)
  println(getAlias.mkString("[", ",", "]"))

  def getAmount: String = {
    "do http get"
  }

  def getAlias: Array[String] = {
    val request = Http("http://localhost:5000/api/productsearch/v1/Alias").header("Authorization", s"Bearer $apiKey")
    val response = request.asString
    Json.parse(response.body).as[Array[String]]
  }

}

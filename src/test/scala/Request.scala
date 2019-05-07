import play.api.libs.json.{JsObject, Json}
import scalaj.http.{Http, HttpRequest}

object Request {

  val config: JsObject = Json.parse(getClass.getResourceAsStream("config.json")).as[JsObject]
  val host: String = config.value("host").as[String]
  val port: String = config.value("port").as[String]

  val apiKey: String = {
    val request = Http("http://" + host + ":" + port + "/api/v1/Auth").param("apiKey", "t1").postData("")
    val response = request.asString
    val json = Json.parse(response.body).as[JsObject]
    json.value("accessToken").as[String]
  }

  def get(path: String): HttpRequest = {
    Http("http://" + host + ":" + port + path).header("Authorization", s"Bearer $apiKey").method("GET")
  }

  def put(path: String, body: String = ""): HttpRequest = {
    Http("http://" + host + ":" + port + path).header("Authorization", s"Bearer $apiKey").postData(body).method("PUT")
  }

  def post(path: String, body: String = ""): HttpRequest = {
    Http("http://" + host + ":" + port + path).header("Authorization", s"Bearer $apiKey").postData(body).method("POST")
  }

  def delete(path: String): HttpRequest = {
    Http("http://" + host + ":" + port + path).header("Authorization", s"Bearer $apiKey").method("DELETE")
  }

}

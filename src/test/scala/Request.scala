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
    request(path, "GET")
  }

  def put(path: String, body: String = ""): HttpRequest = {
    request(path, "PUT", body)
  }

  def post(path: String, body: String = ""): HttpRequest = {
    request(path, "POST", body)
  }

  def delete(path: String): HttpRequest = {
    request(path, "DELETE")
  }

  def request(path: String, method: String): HttpRequest = {
    Http(s"http://$host:$port$path").header("Authorization", s"Bearer $apiKey").method(method)
  }

  def request(path: String, method: String, body: String): HttpRequest = {
    Http(s"http://$host:$port$path").header("Authorization", s"Bearer $apiKey").header("content-type", "application/json").postData(body).method(method)
  }

}

import java.net.ConnectException

import play.api.libs.json.{JsObject, Json}
import scalaj.http.{Http, HttpRequest}

import scala.util.Try

object Request {

  val config: JsObject = Json.parse(getClass.getResourceAsStream("config.json")).as[JsObject]
  val host: String = config.value("host").as[String]
  val port: String = config.value("port").as[String]
  val apiKey: String = config.value("apiKey").as[String]

  val accessToken: String = {
    val request = Http(s"http://$host:$port/api/v1/Auth").param("apiKey", apiKey).postData("")
    val response = Try(request.asString)
    if (response.isFailure) {
      throw new ConnectException("Could not connect to the Hesehus API. Make sure the Docker image is running and you are on SDU/TEK's VPN.")
    }
    val json = Json.parse(response.get.body).as[JsObject]
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
    Http(s"http://$host:$port$path").header("Authorization", s"Bearer $accessToken").method(method)
  }

  def request(path: String, method: String, body: String): HttpRequest = {
    Http(s"http://$host:$port$path").header("Authorization", s"Bearer $accessToken").header("content-type", "application/json").postData(body).method(method)
  }

}

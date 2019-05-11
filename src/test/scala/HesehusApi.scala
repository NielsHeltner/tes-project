import Request._
import play.api.libs.json.{JsArray, Json}
import scalaj.http.HttpResponse

class HesehusApi {

  def getAmount: String = {
    "do http get"
  }

  def getAlias: JsArray = {
    val request = get("/api/productsearch/v1/Alias")
    val response = request.asString
    Json.parse(response.body).as[JsArray]
  }

  def putAlias(indices: Seq[String]): HttpResponse[String] = {
    val indicesJson = Json.toJson(indices).as[JsArray]
    val request = put("/api/productsearch/v1/Alias", Json.stringify(indicesJson))
    //println(s"${request.method} body: " + Json.stringify(indicesJson))
    //request.postData(Json.stringify(indicesJson))
    request.asString
  }

}

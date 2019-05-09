import Request._
import play.api.libs.json.{JsObject, Json}

class HesehusApi {

  def getAmount: String = {
    "do http get"
  }

  def createIndex: (String, Boolean) =  {
    val request = post("/api/productsearch/v1/Index")
    val response = request.asString
    (Json.parse(response.body).as[JsObject].value("id").as[String], response.is2xx)
  }

  def getIndices: List[String] = {
    val request = get("/api/productsearch/v1/Index")
    val response = request.asString

    Json.parse(response.body).as[List[JsObject]].map(obj => obj.value("id").as[String])
  }

  def getAlias: Array[String] = {
    val request = get("/api/productsearch/v1/Alias")
    val response = request.asString
    Json.parse(response.body).as[Array[String]]
  }

}

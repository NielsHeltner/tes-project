import Request._
import play.api.libs.json.{JsObject, Json}
import play.api.libs.json.{JsArray, JsObject, Json}

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

  def removeIndex(index: String): Boolean = {
    val request = delete("/api/productsearch/v1/Index/" + index)
    val response = request.asString
    println("Remove Response code " + response.code)
    response.is2xx
  }

  def getAlias: JsArray = {
    val request = get("/api/productsearch/v1/Alias")
    val response = request.asString
    Json.parse(response.body).as[JsArray]
  }

  def putAlias(indices: Seq[String]): Unit = {
    val indicesJson = Json.toJson(indices).as[JsArray]
    val request = put("/api/productsearch/v1/Alias", Json.stringify(indicesJson))
    //println(s"${request.method} body: " + Json.stringify(indicesJson))
    //request.postData(Json.stringify(indicesJson))
    request.asString
  }

}

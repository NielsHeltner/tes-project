import Request._
import play.api.libs.json.{JsArray, Json, JsObject}
import scalaj.http.HttpResponse

class HesehusApi {

  val jsonGenerator = new JsonGenerator

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

  def postIndexing: String = {
    val body = Json.parse(getClass.getResourceAsStream("postIndexingBody.json")).as[JsObject]
    val generatedJson = jsonGenerator.parseJsObject(body)
    println(Json.prettyPrint(generatedJson))
    val request = post("/api/productsearch/v1/Indexing", generatedJson.toString())
    val response = request.asString
    response.code.toString
  }
}

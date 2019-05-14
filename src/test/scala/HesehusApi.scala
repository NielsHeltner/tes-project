import Request.{getClass, _}
import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString, JsValue, Json}
import org.scalacheck.Gen
import play.api.libs.json.Json.JsValueWrapper

import scala.collection.mutable.ArrayBuffer

class HesehusApi {

  val jsonGenerator = new JsonGenerator

  def getAmount: String = {
    "do http get"
  }

  def getAlias: Array[String] = {
    val request = get("/api/productsearch/v1/Alias")
    val response = request.asString
    Json.parse(response.body).as[Array[String]]
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

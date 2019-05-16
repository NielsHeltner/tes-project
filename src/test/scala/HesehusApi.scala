import Request._
import play.api.libs.json.{JsArray, JsObject, Json}

class HesehusApi {

  def reset(): Unit = {
    println("Resetting SUT...")
    putAlias(Seq[String]())
    val indices = getIndices
    println("Removing " + indices.size + " indices...")
    indices.foreach(removeIndex)
    println("Finished resetting")
  }

  def createIndex: (String, Int) =  {
    val request = post("/api/productsearch/v1/Index")
    val response = request.asString
    (Json.parse(response.body).as[JsObject].value("id").as[String], response.code)
  }

  def getIndices: List[String] = {
    val request = get("/api/productsearch/v1/Index")
    val response = request.asString
    Json.parse(response.body).as[List[JsObject]].map(jsObj => jsObj.value("id").as[String])
  }


  def removeIndex(index: String): Int = {
    val request = delete(s"/api/productsearch/v1/Index/$index")
    val response = request.asString
    response.code
  }

  def getAlias: List[String] = {
    val request = get("/api/productsearch/v1/Alias")
    val response = request.asString
    Json.parse(response.body).as[List[JsObject]].map(jsObj => jsObj.value("id").as[String])
  }

  def putAlias(indices: Seq[String]): Int = {
    val indicesJson = Json.toJson(indices).as[JsArray]
    val request = put("/api/productsearch/v1/Alias", Json.stringify(indicesJson))
    request.asString.code
  }

  def createIndexing(product: JsObject): Int = {
    println(Json.prettyPrint(product))
    val request = post("/api/productsearch/v1/Indexing", Json.stringify(product))
    request.asString.code
  }

  def getIndexing(index: String): JsObject = {
    val request = get(s"/api/productsearch/v1/Indexing/$index")
    val response = request.asString
    Json.parse(response.body).as[JsObject]
  }

  def putIndexing(product: JsObject): Int = {
    val request = put("/api/productsearch/v1/Indexing", Json.stringify(product))
    request.asString.code
  }

  def removeIndexing(index: String): Int = {
    val request = delete(s"/api/productsearch/v1/Indexing/$index")
    request.asString.code
  }
}

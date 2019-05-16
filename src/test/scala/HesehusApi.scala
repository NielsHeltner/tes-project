import Request._
import play.api.libs.json.{JsArray, JsObject, Json}

class HesehusApi {

  def reset(): Unit = {
    println("Resetting SUT...")
    putAlias(List[String]())
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
    val response = request.asString
    println("Response:\n" + response.body + "\n")
    //println(Json.prettyPrint(Json.parse(response.body)))
    response.code
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

  def getProductIndex(index: String, productId: String): JsObject = {
    val request = get(s"/api/productsearch/v1/ProductIndex/$index/$productId")
    val response = request.asString
    Json.parse(response.body).as[JsObject]
  }

  def deleteProductIndex(index: String, productId: String): Int = {
    val request = delete(s"/api/productsearch/v1/ProductIndex/$index/$productId")
    request.asString.code
  }

  def postSearch(generatedJson: JsObject): List[String] =  {
    //val body = Json.parse(getClass.getResourceAsStream("searchAllProductsBody.json")).as[JsObject]
    //val generatedJson = JsonGenerator.parseJsObject(body)
    println(Json.prettyPrint(generatedJson))
    val request = post("/api/productsearch/v1/Search", generatedJson.toString)
    val response = request.asString
    Json.parse(response.body).as[List[JsObject]].map(jsObj => jsObj.value("id").as[String])
  }

}

import Request._
import play.api.libs.json.{JsArray, JsObject, Json}
import scalaj.http.HttpResponse

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

  def postAlias(indices: Seq[String]): Int = {
    val indicesJson = Json.toJson(indices).as[JsArray]
    val request = post("/api/productsearch/v1/Alias/add", Json.stringify(indicesJson))
    request.asString.code
  }

  def removeAlias(indices: Seq[String]): Int = {
    val indicesJson = Json.toJson(indices).as[JsArray]
    val request = post("/api/productsearch/v1/Alias/remove", Json.stringify(indicesJson))
    request.asString.code
  }

  def createIndexing(product: JsObject): HttpResponse[String] = {
    val request = post("/api/productsearch/v1/Indexing", Json.stringify(product))
    val response = request.asString
    response
  }

  def getIndexing(index: String): HttpResponse[String] = {
    val request = get(s"/api/productsearch/v1/Indexing/$index")
    val response = request.asString
    response
  }

  def putIndexing(product: JsObject): HttpResponse[String] = {
    val request = put("/api/productsearch/v1/Indexing", Json.stringify(product))
    request.asString
  }

  def removeIndexing(index: String): HttpResponse[String] = {
    val request = delete(s"/api/productsearch/v1/Indexing/$index")
    request.asString
  }

  def postProductIndex(index: String, product: JsObject): HttpResponse[String] = {
    val request = post(s"/api/productsearch/v1/ProductIndex/$index", Json.stringify(product))
    val response = request.asString
    response
  }

  def getProductIndex(index: String, productId: String): HttpResponse[String] = {
    val request = get(s"/api/productsearch/v1/ProductIndex/$index/$productId")
    request.asString
  }

  def deleteProductIndex(index: String, productId: String): HttpResponse[String] = {
    val request = delete(s"/api/productsearch/v1/ProductIndex/$index/$productId")
    request.asString
  }

  def upsertBulk(index: String, productIds: Seq[JsObject]): HttpResponse[String] = {
    val request = post(s"/api/productsearch/v1/Bulk/upsert/$index", Json.stringify(Json.toJson(productIds)))
    request.asString
  }

  def getBulk(index: String, productIds: Seq[String]): HttpResponse[String] = {
    val request = post(s"/api/productsearch/v1/Bulk/$index", Json.stringify(Json.toJson(productIds)))
    request.asString
  }

  def deleteBulk(index: String, productIds: Seq[String]): HttpResponse[String] = {
    val request = post(s"/api/productsearch/v1/Bulk/delete/$index", Json.stringify(Json.toJson(productIds)))
    request.asString
  }

  def search(generatedJson: JsObject): HttpResponse[String] =  {
    Thread.sleep(1000)
    val request = post("/api/productsearch/v1/Search", Json.stringify(generatedJson))
    request.asString
  }

}

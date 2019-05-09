import Request._
import play.api.libs.json.{JsObject, Json}

object CleanUp {
  def cleanUp {
    println("Cleaning sut...")

    cleanIndices
  }

  def cleanIndices {
    val request = get("/api/productsearch/v1/Index")
    val response = request.asString
    val indices = Json.parse(response.body).as[Array[JsObject]].map(obj => obj.value("id").as[String])
    indices.foreach(index => delete("/api/productsearch/v1/Index/" + index).asString.code)
  }
}
import Request._
import play.api.libs.json.Json

class HesehusApi {

  def getAmount: String = {
    "do http get"
  }

  def getAlias: Array[String] = {
    val request = get("/api/productsearch/v1/Alias")
    val response = request.asString
    Json.parse(response.body).as[Array[String]]
  }

}

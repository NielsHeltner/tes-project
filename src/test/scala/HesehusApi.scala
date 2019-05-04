import scalaj.http.Http
import spray.json._

class HesehusApi {

  val apiKey: String = {
    val request = Http("http://localhost:5000/api/v1/Auth").param("apiKey", "t1").postData("")
    val response = request.asString
    val json = response.body.parseJson.asJsObject
    val apiKeyWithQuotes = json.fields("accessToken").toString()
    apiKeyWithQuotes.substring(1, apiKeyWithQuotes.length - 1)
  }
  println(apiKey)

  def getAmount: String = {
    "do http get"
  }

}

import org.scalacheck.Gen
import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString, Json}

class JsonGenerator {

  def parseJsObject(jsObject: JsObject): JsObject = {
    jsObject.keys.foldLeft(JsObject.empty)((jObj, key) => {
      jsObject.value(key) match {
        case arr: JsArray =>
          jObj ++ Json.obj(key -> parseJsArray(arr))
        case obj: JsObject =>
          jObj ++ Json.obj(key -> parseJsObject(obj))
        case _: JsNumber =>
          val value = generateNumber
          jObj ++ Json.obj(key -> value)
        case _: JsString =>
          val value = generateString
          jObj ++ Json.obj(key -> value)
        case _ =>
          println("ERROR - Could not parse following key " + key)
          JsObject.empty
      }
    })
  }

  def parseJsArray(jsArray: JsArray): JsArray = {
    var array = JsArray.empty
    for (element <- jsArray.value) {
      element match {
        case arr:JsArray =>
          array = array :+ parseJsArray(arr)
        case obj:JsObject =>
          array = array :+ parseJsObject(obj)
        case _:JsNumber =>
          array = array :+ JsNumber(generateNumber)
        case _:JsString =>
          array = array :+ JsString(generateString)
        case _ =>
          println("ERROR - Could not parse following element in array " + element.toString())
      }
    }
    array
  }

  def generateString: String = {
    Gen.asciiPrintableStr.sample.get
  }

  def generateNumber: Int = {
    Gen.chooseNum(Int.MinValue,Int.MaxValue).sample.get
  }
}

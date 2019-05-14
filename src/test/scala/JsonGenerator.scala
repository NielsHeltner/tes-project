import org.joda.time.{DateTime, DateTimeZone}
import org.scalacheck.Gen
import play.api.libs.json._

object JsonGenerator {

  def parseJsObject(jsObject: JsObject): JsObject = {
    jsObject.keys.foldLeft(JsObject.empty)((jObj, key) => {
      jsObject.value(key) match {
        case arr: JsArray =>
          jObj ++ Json.obj(key -> parseJsArray(arr))
        case obj: JsObject =>
          jObj ++ Json.obj(key -> parseJsObject(obj))
        case number: JsNumber =>
          var value: Double = 0.0
          if (number.value.isDecimalDouble) {
            value = generateDouble
          }
          else { //number.value.isValidInt
            value = generateInteger
          }
          jObj ++ Json.obj(key -> value)
        case string: JsString =>
          var value = ""
          if (string.value == "date") {
            value = generateDate
          }
          else if (string.value.isEmpty) {
            value = generateString()
          }
          else {
            val size = string.value.toInt
            value = generateString(size)
          }
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
        case arr: JsArray =>
          array = array :+ parseJsArray(arr)
        case obj: JsObject =>
          array = array :+ parseJsObject(obj)
        case number: JsNumber =>
          var value: Double = 0.0
          if (number.value.isDecimalDouble) {
            value = generateDouble
          }
          else { //number.value.isValidInt
            value = generateInteger
          }
          array = array :+ JsNumber(value)
        case string: JsString =>
          var value = ""
          if (string.value == "date") {
            value = generateDate
          }
          else if (string.value.isEmpty) {
            value = generateString()
          }
          else {
            val size = string.value.toInt
            value = generateString(size)
          }
          array = array :+ JsString(value)
        case _ =>
          println("ERROR - Could not parse following element in array " + element.toString())
      }
    }
    array
  }

  def generateString(size: Int = Int.MaxValue): String = {
    Gen.asciiPrintableStr.map(string => if (string.length > size) string.substring(0, size) else string).sample.get
  }

  def generateDate: String = {
    val date = Gen.calendar.sample.get
    val datetime = new DateTime(date).withZone(DateTimeZone.UTC)
    datetime.toString
  }

  def generateDouble: Double = {
    Gen.frequency(
      (8, Gen.chooseNum(Double.MinValue, Double.MaxValue)),
      (3, Gen.chooseNum(Float.MinValue, Float.MaxValue)),
      (1, Gen.oneOf(Double.MinValue, Float.MinValue, -0.99, 0.0, 0.99, Float.MaxValue, Double.MaxValue))
    ).sample.get.asInstanceOf[Double]
  }

  def generateInteger: Int = {
    Gen.frequency(
      (8, Gen.chooseNum(Int.MinValue, Int.MaxValue)),
      (3, Gen.chooseNum(Short.MinValue, Short.MaxValue)),
      (1, Gen.oneOf(Int.MinValue, Short.MinValue, -1, 0, 1, Short.MaxValue, Int.MaxValue))
    ).sample.get.asInstanceOf[Int]
  }

}

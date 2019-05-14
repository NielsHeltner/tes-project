import org.joda.time.{DateTime, DateTimeZone}
import org.scalacheck.Gen
import play.api.libs.json._

object JsonGenerator {

  def parseJs(js: JsValue, acc: JsValue = JsObject.empty, key: String = ""): JsValue = {
    val value: JsValue = js match {
      case arr: JsArray =>
        arr.value.foldLeft(JsArray.empty)((jsArr, value) =>
          parseJs(value, jsArr).as[JsArray])
      case obj: JsObject =>
        obj.keys.foldLeft(JsObject.empty)((jsObj, key) =>
          parseJs(obj.value(key), jsObj, key).as[JsObject])
      case number: JsNumber =>
        JsNumber(generateNumber(number))
      case string: JsString =>
        JsString(generateString(string))
      case _ =>
        println("ERROR - Could not parse JS value")
        JsNull
    }

    acc match {
      case arr: JsArray =>
        arr :+ value
      case obj: JsObject =>
        if (key.isEmpty) {
          value
        }
        else {
          obj ++ Json.obj(key -> value)
        }
      case _ =>
        println("ERROR - Could not parse JS value")
        JsNull
    }
  }

  def generateString(jsStr : JsString): String = {

    def generateSizedString(size: Int = Int.MaxValue): String = {
      Gen.asciiPrintableStr.map(string => if (string.length > size) string.substring(0, size) else string).sample.get
    }

    def generateDate: String = {
      val date = Gen.calendar.sample.get
      val datetime = new DateTime(date).withZone(DateTimeZone.UTC)
      datetime.toString
    }

    if (jsStr.value == "date") {
      generateDate
    }
    else if (jsStr.value.isEmpty) {
      generateSizedString()
    }
    else {
      val size = jsStr.value.toInt
      generateSizedString(size)
    }
  }

  def generateNumber(jsNum: JsNumber): Double = {

    def generateDouble: Double = {
      Gen.frequency(
        (8, Gen.chooseNum(Double.MinValue, Double.MaxValue)),
        (1, Gen.oneOf(Double.MinValue, -0.99d, 0.0d, 0.99d, Double.MaxValue))
      ).sample.get.asInstanceOf[Double]
    }

    def generateInteger: Int = {
      Gen.frequency(
        (8, Gen.chooseNum(Int.MinValue, Int.MaxValue)),
        (1, Gen.oneOf(Int.MinValue, -1, 0, 1, Int.MaxValue))
      ).sample.get.asInstanceOf[Int]
    }

    if (jsNum.value.isValidInt) {
      generateInteger
    }
    else { //jsNum.value.isDecimalDouble
      generateDouble
    }
  }

}

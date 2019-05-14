import org.joda.time.{DateTime, DateTimeZone}
import org.scalacheck.{Arbitrary, Gen}
import play.api.libs.json._

object JsonGenerator {

  def parseJs[T <: JsValue](js: JsValue, acc: T = JsObject.empty, key: String = ""): T = {
    val value: JsValue = js match {
      case arr: JsArray =>
        arr.value.foldLeft(JsArray.empty)((jsArr, value) =>
          parseJs(value, jsArr))
      case obj: JsObject =>
        obj.keys.foldLeft(JsObject.empty)((jsObj, key) =>
          parseJs(obj.value(key), jsObj, key))
      case number: JsNumber =>
        JsNumber(generateNumber(number))
      case string: JsString =>
        JsString(genString(string).sample.get)
      case _: JsBoolean =>
        JsBoolean(generateBoolean)
      case _ =>
        println("ERROR - Could not parse JS value")
        JsNull
    }

    acc match {
      case arr: JsArray =>
        (arr :+ value).asInstanceOf[T]
      case obj: JsObject =>
        if (key.isEmpty) {
          value.asInstanceOf[T]
        }
        else {
          (obj ++ Json.obj(key -> value)).asInstanceOf[T]
        }
      case _ =>
        println("ERROR - Could not parse JS value")
        JsNull.asInstanceOf[T]
    }
  }

  def genString(jsString : JsString): Gen[String] = {

    def genSizedString(size: Int = Int.MaxValue): Gen[String] = {
      Gen.asciiPrintableStr.map(string => if (string.length > size) string.substring(0, size) else string)
    }

    def genDate: Gen[String] = {
      val date = Gen.calendar.sample.get
      new DateTime(date).withZone(DateTimeZone.UTC).toString
    }

    if (jsString.value == "date") {
      genDate
    }
    else if (jsString.value.isEmpty) {
      genSizedString()
    }
    else {
      val size = jsString.value.toInt
      genSizedString (size)
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

  def generateBoolean: Boolean = {
    Arbitrary.arbBool.arbitrary.sample.get
  }

}

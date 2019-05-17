import org.joda.time.DateTime
import play.api.libs.json.{JsBoolean, JsNumber, JsObject, JsString}

class SearchFilter {

  var filteredElements: Seq[JsObject] = _

  def filter(searchParams: JsObject, elements: Seq[JsObject]): Seq[JsObject] = {
    filteredElements = elements
    println("Size before filter: " + filteredElements.size)

    filterOutOfStock(searchParams)
    println("Size after out of stock filter: " + filteredElements.size)
    filterIncludeInactive(searchParams)
    println("Size after inactive filter: " + filteredElements.size)

    filteredElements.sortBy(js => js.value("id").as[String])
  }

  def filterOutOfStock(params: JsObject): Unit = {
    if(!params.value("showOutOfStockProducts").as[JsBoolean].value)
      filteredElements = filteredElements.filterNot(element => element.value("stock").as[JsNumber].value.doubleValue() == 0)
  }

  def filterIncludeInactive(params: JsObject): Unit = {
    if(!params.value("includeInActive").as[JsBoolean].value) {
      filteredElements = filteredElements.filter(element => {
        val from = new DateTime(element.value("activeFrom").as[JsString].value).getMillis
        val to = new DateTime(element.value("activeTo").as[JsString].value).getMillis
        val time = new DateTime(params.value("searchTime").as[JsString].value)

        println("Time millis from: " + from)
        println("Is after: " + time.isAfter(from))
        println("Time millis searchParam: " + time)
        println("Is before: " + time.isBefore(to))
        println("Time millis to: " + to)

        time.isAfter(from) && time.isBefore(to)
      })
    }
  }
}

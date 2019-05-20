import org.joda.time.DateTime
import play.api.libs.json.{JsBoolean, JsNumber, JsObject, JsString}

class SearchFilter {

  var filteredElements: Seq[JsObject] = _

  def filter(searchParams: JsObject, elements: Seq[JsObject]): Seq[JsObject] = {
    filteredElements = elements

    filterOutOfStock(searchParams)
    filterIncludeInactive(searchParams)

    filteredElements.sortBy(js => js.value("id").as[String])
  }

  def filterOutOfStock(params: JsObject): Unit = {
    if(!params.value("showOutOfStockProducts").as[JsBoolean].value)
      filteredElements = filteredElements.filterNot(element => element.value("stock").as[JsNumber].value.doubleValue() == 0)
  }

  def filterIncludeInactive(params: JsObject): Unit = {
    if(!params.value("includeInActive").as[JsBoolean].value) {
      filteredElements = filteredElements.filter(element => {
        val from = new DateTime(element.value("activeFrom").as[JsString].value).getMillis - 1
        val to = new DateTime(element.value("activeTo").as[JsString].value).getMillis + 1
        val time = new DateTime(params.value("searchTime").as[JsString].value)

        time.isAfter(from) && time.isBefore(to)
      })
    }
  }
}

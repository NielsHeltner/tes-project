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

  def filterOutOfStock(params: JsObject): Seq[JsObject] = {
    filteredElements.filter(element => (element.value("stock").as[JsNumber].value.doubleValue() == 0) && params.value("showOutOfStockProducts").as[JsBoolean].value)
  }

  def filterIncludeInactive(params: JsObject): Seq[JsObject] = {
    filteredElements.filter(element => {
      val from = new DateTime(element.value("activeFrom").as[JsString].value).getMillis
      val to = new DateTime(element.value("activeTo").as[JsString].value).getMillis
      val time = new DateTime(params.value("searchTime"))

      time.isAfter(from) &&
      time.isBefore(to) &&
      element.value("includeInActive").as[JsBoolean].value
    })
  }
}

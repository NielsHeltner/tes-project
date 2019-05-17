import org.joda.time.{DateTime, DateTimeZone}
import org.scalacheck.{Arbitrary, Gen}
import play.api.libs.json._

object JsonGen {

  def genAttribute: Gen[JsObject] = for {
    attributeKey <- genSizedString(max = 50)
    name <- genSizedString(max = 50)
    values <- Gen.nonEmptyListOf(for {
      id <- genSizedString(max = 50)
      value <- genSizedString()
    } yield Json.obj("id" -> id, "value" -> value))
  } yield Json.obj("attributeKey" -> attributeKey, "name" -> name, "values" -> values)

  def genSizedIndexingJson(size: Int = 1001): Gen[JsObject] = {
    genIndexingJson.retryUntil(json => {
      val sz = allKeys(json).size
      println("Sizze: " + sz)
      sz <= size
    })
  }

  def allKeys(json: JsValue): List[String] = json match {
    case o: JsObject => o.keys.toList ++ o.values.flatMap(allKeys)
    case arr: JsArray => arr.value.flatMap(allKeys).toList
    case _ => List()
  }

  def genIndexingJson: Gen[JsObject] = for {
    id <- genSizedString(max = 50)
    alternativeIds <- Gen.nonEmptyListOf(genSizedString(max = 50))
    productNumber <- genSizedString(max = 50)
    dataAge <- genDate()
    name <- genSizedString(max = 255)
    categories <- Gen.nonEmptyListOf(for {
      categoryId <- genSizedString(max = 50)
      name <- genSizedString(max = 255)
    } yield Json.obj("categoryId" -> categoryId, "name" -> name)
    )
    stockKeepingUnits <- Gen.nonEmptyListOf(for {
      id <- genSizedString(max = 50)
      skuNo <- genSizedString(max = 255)
      ean <- genSizedString(max = 13)
      name <- genSizedString(max = 255)
      attributes <- Gen.nonEmptyListOf(genAttribute)
      metaData <- genSizedString()
    } yield Json.obj(
        "id" -> id,
        "skuNo" -> skuNo,
        "ean" -> ean,
        "name" -> name,
        "attributes" -> attributes,
        "metaData" -> metaData
      )
    )
    media <- Gen.nonEmptyListOf(for {
      _type <- genInt(3)
      url <- genSizedString()
      metaData <- genSizedString()
    } yield Json.obj("type" -> _type, "url" -> url, "metaData" -> metaData)
    )
    prices <- for {
      currency <- genSizedString(max = 3)
      defaultPrice <- genDouble()
      salesPrices <- Gen.nonEmptyListOf(for {
        priceGroupId <- genSizedString()
        price <- genDouble()
        offerTimeFromInclusive <- genDate()
        offerTimeToExclusive <- genDate(offerTimeFromInclusive)
      } yield Json.obj(
          "priceGroupId" -> priceGroupId,
          "price" -> price,
          "offerTimeFromInclusive" -> offerTimeFromInclusive.toString,
          "offerTimeToExclusive" -> offerTimeToExclusive.toString
        )
      )
    } yield Json.obj("currency" -> currency, "defaultPrice" -> defaultPrice, "salesPrices" -> salesPrices)
    shortDescription <- genSizedString(max = 255)
    longDescription <- genSizedString()
    alternativeSearchWords <- Gen.nonEmptyListOf(genSizedString())
    attributes <- Gen.nonEmptyListOf(genAttribute)
    metaData <- genSizedString()
    rating <- genDouble(max = 5.0d)
    stock <- genDouble(max = 999999999999.99d)
    activeFrom <- genDate()
    activeTo <- genDate(activeFrom)
  } yield Json.obj(
      "id" -> id,
      "alternativeIds" -> alternativeIds,
      "productNumber" -> productNumber,
      "dataAge" -> dataAge.toString,
      "name" -> name,
      "categories" -> Json.arr(categories),
      "stockKeepingUnits" -> stockKeepingUnits,
      "media" -> media,
      "prices" -> prices,
      "shortDescription" -> shortDescription,
      "longDescription" -> longDescription,
      "alternativeSearchWords" -> alternativeSearchWords,
      "attributes" -> attributes,
      "metaData" -> metaData,
      "rating" -> rating,
      "stock" -> stock,
      "activeFrom" -> activeFrom.toString,
      "activeTo" -> activeTo.toString
    )

  def genSizedString(min: Int = 1, max: Int = Int.MaxValue): Gen[String] = {
    Gen.asciiPrintableStr.retryUntil(string => string.length >= min && string.length <= max && !string.trim.isEmpty &&
      !string.trim.startsWith(".") && !string.trim.endsWith("."))
  }

  def genDate(fromDate: DateTime = new DateTime(-62135751600000L).withZone(DateTimeZone.UTC)): Gen[DateTime] = {
    Gen.calendar.map(new DateTime(_).withZone(DateTimeZone.UTC))
      .map(date => if (fromDate.getYear == 4000) fromDate.plusMillis(1) else date)
      .retryUntil(date => date.getYear > 0 && date.getYear < 10000 && date.isAfter(fromDate))
  }

  def genDouble(max: Double = 9999999999999.99d): Gen[BigDecimal] = for {
    value <- Gen.frequency(
      (8, Gen.chooseNum(0.0d, max)), // stock max = 999999999999.99d (9.99e+11) -- price max = 9999999999999.99d (9.99e+12)
      (1, Gen.oneOf(0.0d, max))
    ).retryUntil(_ >= 0.0d)
  } yield BigDecimal(value).setScale(2, BigDecimal.RoundingMode.HALF_UP)

  def genInt(max: Int = Int.MaxValue): Gen[Int] = for {
    value <- Gen.frequency(
      (8, Gen.chooseNum(0, max)),
      (1, Gen.oneOf(0, 1, max))
    ).retryUntil(_ >= 0)
  } yield value

  def genBoolean: Gen[Boolean] = {
    Arbitrary.arbBool.arbitrary
  }

}

import play.api.libs.json.JsObject

case class Model(alias: Seq[String] = Seq[String](),
                indices: Seq[String] = Seq[String](),
                 products: Seq[JsObject] = Seq[JsObject]()
                ) {}

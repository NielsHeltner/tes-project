import play.api.libs.json.JsObject

case class Model(alias: String,
                indices: Seq[String] = Seq[String](),
                 products: Seq[JsObject] = Seq[JsObject]()
                ) {}

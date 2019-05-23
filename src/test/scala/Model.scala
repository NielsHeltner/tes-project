import play.api.libs.json.JsObject

case class Model(alias: Seq[String] = Seq[String](),
                 indices: Map[String, Seq[JsObject]] = Map[String, Seq[JsObject]]()) {

  def containsProducts: Boolean = indices.values.flatten.nonEmpty

  def aliasContainsProducts: Boolean = indices(alias.head).nonEmpty

  def indicesWithProducts: Seq[String] = indices.filter(indexToProducts => indexToProducts._2.nonEmpty).keys.toSeq

  def currentIndices: Seq[JsObject] = indices(alias.head)

}

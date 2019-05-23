import play.api.libs.json.JsObject
import scala.collection.immutable.HashMap

case class Model(alias: Seq[String] = Seq[String](),
                 indices: HashMap[String, Seq[JsObject]] = HashMap[String, Seq[JsObject]]()) {

  def containsProducts: Boolean = indices.values.flatten.nonEmpty

  def aliasContainsProducts: Boolean = indices(alias.head).nonEmpty

  def indicesWithProducts: Seq[String] = indices.filter(indexAndObjects => indexAndObjects._2.nonEmpty).keys.toSeq

  def currentIndices: Seq[JsObject] = indices(alias.head)

}

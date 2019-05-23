import play.api.libs.json.JsObject
import scala.collection.immutable.HashMap

case class Model(alias: Seq[String] = Seq[String](),
                 indices: HashMap[String, Set[JsObject]] = HashMap[String, Set[JsObject]]()) {

  def containsProducts: Boolean = indices.values.flatten.nonEmpty

  def aliasContainsProducts: Boolean = indices(alias.head).nonEmpty

  def indicesWithProducts: Seq[String] = indices.filter(indexAndObjects => indexAndObjects._2.nonEmpty).keys.toSeq

  def currentIndices: Set[JsObject] = indices(alias.head)

}

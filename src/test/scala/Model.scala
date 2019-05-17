import play.api.libs.json.JsObject

import scala.collection.immutable

case class Model(alias: Seq[String] = Seq[String](),
                 indices: immutable.HashMap[String, immutable.Set[JsObject]] = new immutable.HashMap[String, immutable.Set[JsObject]]
                ) {

  def containsProducts: Boolean = {
    indices.values.flatten.nonEmpty
  }

  def aliasContainsProducts: Boolean = {
    indices(alias.head).nonEmpty
  }

  def indicesWithProducts: Seq[String] = {
    indices.filter(k=> k._2.nonEmpty).keys.toSeq
  }

  def currentIndices: Set[JsObject] = indices(alias.head)

}

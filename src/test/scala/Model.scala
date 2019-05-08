import scala.collection.mutable.ArrayBuffer

class Model {

  var alias: Seq[String] = Seq[String]()
  var indices: Seq[String] = Seq[String]("product-index-t1-2019-05-07-17-54-47-7001334", "product-index-t1-2019-05-07-15-20-49-9740928")

  def isEmpty: Boolean = {
    true
  }

  def clone(alias: Seq[String] = alias, indices: Seq[String] = indices): Model = {
    val clone = new Model
    clone.alias = Seq(alias: _*)
    clone.indices = Seq(indices: _*)
    clone
  }

}

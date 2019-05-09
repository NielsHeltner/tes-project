
class Model {

  var indices = Seq[String]()

  def isEmpty: Boolean = {
    true
  }

  def getAmount: String = {
    "do http get"
  }

  def containIndices: Boolean = {
    indices.nonEmpty
  }

  def getIndices: Seq[String] = {
    indices
  }

  def addIndex(index: String): Model = {
    indices = indices :+ index
    this
  }

}


class Model {

  var indices = List[String]()

  def isEmpty: Boolean = {
    true
  }

  def getAmount: String = {
    "do http get"
  }

  def containIndices: Boolean = {
    indices.nonEmpty
  }

  def getIndices: List[String] = {
    indices
  }

  def addIndex(index: String): Model = {
    indices = indices :+ index
    this
  }

}

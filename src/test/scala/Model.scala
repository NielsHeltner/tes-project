import scala.collection.mutable.ListBuffer

class Model {

  var indices = ListBuffer[String]()

  def isEmpty: Boolean = {
    true
  }

  def getAmount: String = {
    "do http get"
  }

  def containIndices: Boolean = {
    indices.nonEmpty
  }

  def getIndex(index: Int): String = {
    indices(index)
  }

  def getIndices: List[String] = {
    indices.toList
  }

  def addIndex(index: String): Model = {
    /*indices = */indices.append(index)
    this
  }

  def removeIndex(index: String): Model = {
    indices -= index
    this
  }

}

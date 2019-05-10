class Model {

  var alias: Seq[String] = Seq[String]()
  var indices: Seq[String] = Seq[String]()

  def isEmpty: Boolean = {
    true
  }

  def clone(alias: Seq[String] = alias, indices: Seq[String] = indices): Model = {
    val clone = new Model
    clone.alias = Seq(alias: _*)
    clone.indices = Seq(indices: _*)
    clone
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
    indices = indices:+index
    this
  }

  def setAlias(alias: Seq[String]): Model = {
    this.alias = alias
    this
  }

  def removeIndex(index: String): Model = {
    indices = indices.filterNot(item => item.equals(index))
    this
  }

}

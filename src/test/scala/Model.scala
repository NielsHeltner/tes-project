
case class Model(

  var alias: Seq[String] = Seq[String](),
  var indices: Seq[String] = Seq[String](/*"product-index-t1-2019-05-10-15-43-59-6963664", "product-index-t1-2019-05-11-10-39-18-1131747"*/)

) {

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

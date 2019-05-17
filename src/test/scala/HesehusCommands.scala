import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import scala.collection.immutable
import scala.util.Try

//https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md#stateful-testing
//https://github.com/rickynils/scalacheck/tree/master/examples/commands-nix
object HesehusSpecification extends Commands {

  /** The abstract state type. Must be immutable. The [[State]] type should model the state of the system under test (SUT).
    * It should only contain details needed for specifying our pre- and postconditions, and for creating [[Sut]] instances.
    */
  override type State = Model

  override type Sut = HesehusApi

  override def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[Sut]): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  /** The precondition for the initial state, when no commands yet have run. This is used by ScalaCheck when command
    * sequences are shrunk and the first state might differ from what is returned from [[genInitialState]].
    */
  override def initialPreCondition(state: State): Boolean = true

  /** Create a new [[Sut]] instance with an internal state that corresponds to the provided abstract state instance.
    * The provided state is guaranteed to fulfill [[initialPreCondition]], and [[newSut]] will never be called if
    * [[canCreateNewSut]] is not true for the given state.
    */
  override def newSut(state: State): Sut = {
    val api = new HesehusApi
    api.putAlias(state.alias)
    api
  }

  /** Destroy the system represented by the given [[Sut]] instance, and release any resources related to it.
    */
  override def destroySut(sut: Sut): Unit = sut.reset()

  /** A generator that should produce an initial [[State]] instance that is usable by [[newSut]] to create a new system
    * under test. The state returned by this generator is always checked with the [[initialPreCondition]] method before
    * it is used.
    */
  override def genInitialState: Gen[State] = {
    def genInitialIndices: Gen[immutable.HashMap[String, immutable.Set[JsObject]]] = for {
      size <- Gen.choose(1, 10)
    } yield 1.to(size).foldLeft(new immutable.HashMap[String, immutable.Set[JsObject]])((acc, _) => acc + (new HesehusApi().createIndex._1 -> new immutable.HashSet[JsObject]()))

    def genInitialAlias(indices: immutable.HashMap[String, immutable.Set[JsObject]]): Gen[List[String]] = {
      Gen.listOfN(1, Gen.oneOf(indices.keys.toSeq))
    }

    for {
      indices <- genInitialIndices
      alias <- genInitialAlias(indices)
    } yield {
      Model(indices = indices, alias = alias)
    }
  }

  def genPutAlias(state: State): Gen[PutAlias] = {
    Gen.listOfN(1, Gen.oneOf(state.indices.keys.toSeq)).map(PutAlias)
  }

  def genRemoveIndex(state: State): Gen[RemoveIndex] = {
    Gen.oneOf(state.indices.keys.toSeq).map(RemoveIndex)
  }

  /*def genSearch(state: State): Gen[PostSearch] = {
    val body = Json.parse(getClass.getResourceAsStream("searchAllProductsBody.json")).as[JsObject]
    for {
      json <- JsonGen.genJson(body)
    } yield PostSearch(json)
  }*/

  def genCreateIndexing(state: State): Gen[CreateIndexing] = {
    for {
      json <- JsonGen.genPostIndexingJson
    } yield CreateIndexing(json)
  }

  def genGetIndexing(state: State): Gen[GetIndexing] = {
    Gen.oneOf(state.indices(state.alias.head).toSeq).map(GetIndexing)
  }

  def genPutIndexing(state: State): Gen[PutIndexing] = {
    for {
      json <- JsonGen.genPostIndexingJson
      product <- Gen.oneOf(state.indices(state.alias.head).toSeq)
    } yield PutIndexing(json ++ Json.obj("id" -> product.value("id"))) // json.value("id") = product.value("id") ???
  }

  def genRemoveIndexing(state: State): Gen[RemoveIndexing] = {
    Gen.oneOf(state.indices(state.alias.head).toSeq).map(RemoveIndexing)
  }

  def genGetProductIndex(state: State): Gen[GetProductIndex] = {
    for {
      index <- Gen.oneOf(state.indices.keys.toSeq)
      product <- Gen.oneOf(state.indices(index).toSeq)
    }
      yield GetProductIndex(index, product)
  }

  def genDeleteProductIndex(state: State): Gen[DeleteProductIndex] = {
    for {
      index <- Gen.oneOf(state.indices.keys.toSeq)
      product <- Gen.oneOf(state.indices(index).toSeq)
    }
      yield DeleteProductIndex(index, product)
  }

  /** A generator that, given the current abstract state, should produce a suitable Command instance.
    */
  override def genCommand(state: State): Gen[Command] = {
    var cmds = Seq[Gen[Command]]()
    cmds = cmds ++ Seq[Gen[Command]](
      Gen.const(GetAlias())
    )
    if (state.indices.nonEmpty) {
      cmds = cmds ++ Seq[Gen[Command]](
        genRemoveIndex(state),
        genPutAlias(state)
      )
    }
    if (state.alias.nonEmpty) {
      cmds = cmds ++ Seq[Gen[Command]] (
        genCreateIndexing(state)
      )
      if (state.aliasContainsProducts) {
        println("Yo i got products!")
        cmds = cmds ++ Seq[Gen[Command]] (
          genGetIndexing(state)
          //genPutIndexing(state),
          //genRemoveIndexing(state)
        )
      }
    }
    Gen.oneOf(Gen.const(CreateIndex()), Gen.const(GetIndices()), cmds: _*)
  }

  case class CreateIndex() extends Command {

    var response: (String, Int) = ("-1", -1)

    override type Result = Int

    override def run(sut: Sut): Result = {
      response = new HesehusApi().createIndex
      response._2
    }

    override def nextState(state: State): State = {
      if (response._1 == "-1") { // [[run]] hasn't run yet -- no state update
        state
      }
      else {
        state.copy(indices = state.indices + (response._1 -> new immutable.HashSet[JsObject]()))
      }
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get == 200
      if (!success) {
        println("CreateIndex")
        println("  " + result.get)
      }
      success
    }
  }

  case class GetIndices() extends Command {

    override type Result = List[String]

    override def run(sut: Sut): Result = {
      sut.getIndices
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = state.indices.keys.toSeq.sorted == result.get.sorted
      if (!success) {
        println("GetIndices")
        println("  State: " + state.indices + "\n")
        println("  API: " + result.get + "\n")
      }
      success
    }
  }

  case class RemoveIndex(index: String) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.removeIndex(index)

    override def nextState(state: State): State = state.copy(indices = state.indices - index, alias = state.alias.filterNot(_ == index))

    override def preCondition(state: State): Boolean = state.indices.contains(index)

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get == 200
      if (!success) {
        println("RemoveIndex")
        println("  " + result.get)
      }
      success
    }
  }

  case class GetAlias() extends Command {

    override type Result = List[String]

    override def run(sut: Sut): Result = sut.getAlias

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = !(result.get.size > 1) && result.get.sorted == state.alias.sorted
      if (!success) {
        println("GetAlias")
        println("  State: " + state.alias)
        println("  API: " + result.get)
      }
      success
    }
  }

  case class PutAlias(indices: List[String]) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.putAlias(indices)

    override def nextState(state: State): State = state.copy(alias = indices)

    override def preCondition(state: State): Boolean = indices.forall(state.indices.contains)

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get == 200
      if (!success) {
        println("PutAlias")
        println("  " + result.get)
      }
      success
    }
  }

  case class PostSearch(generatedJson: JsObject) extends Command {

    override type Result = List[String]

    override def run(sut: Sut): Result = sut.postSearch(generatedJson)

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val searchFilter = new SearchFilter
      val filteredProducts = searchFilter.filter(generatedJson, state.indices(state.alias.head).toSeq)
      val sortedResult = result.get.sorted
      val success = filteredProducts.size == result.get.size &&
        filteredProducts.indices.count(index => filteredProducts(index).as[JsObject].value("id").as[String] != sortedResult(index)) == 0
      if (!success) {
        println("PostSearch")
        println("  State: " + state.alias)
        println("  API: " + result.get)
      }
      success
    }
  }

  case class CreateIndexing(product: JsObject) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = {
      sut.createIndexing(product)
    }

    override def nextState(state: State): State = {
      state.copy(indices = state.indices + (state.alias.head -> (state.indices(state.alias.head) + product)))
    }

    override def preCondition(state: State): Boolean = state.alias.nonEmpty

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get == 200
      if (!success) {
        println("CreateIndexing")
        println("  " + result.get)
      }
      success
    }
  }

  case class GetIndexing(product: JsObject) extends Command {

    override type Result = JsObject

    override def run(sut: Sut): Result = sut.getIndexing(product.value("id").as[String])

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = state.alias.nonEmpty

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val updated_result = result.get - "isInStock"
      //val success = sortJs(product).toString() == sortJs(updated_result).toString()
      val success = product.value("id") == updated_result.value("id")
      if (!success) {
        println("GetIndexing")
        println("SUT:  " + Json.prettyPrint(sortJs(updated_result)))
        println("STATE:  " + Json.prettyPrint(sortJs(product)))
      }
      success
    }
  }

  case class PutIndexing(product: JsObject) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.putIndexing(product)

    override def nextState(state: State): State = {
      val toReplace = state.indices(state.alias.head).toSeq.find(prod => prod.value("id").as[String] == product.value("id").as[String])
      if (toReplace.isDefined) {
        state.copy(indices = state.indices + (state.alias.head -> (state.indices(state.alias.head) + toReplace.get)))
      } else {
        state
      }
    }

    override def preCondition(state: State): Boolean = state.alias.nonEmpty

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get == 200
      if (!success) {
        println("PutIndexing")
        println("  " + result.get)
      }
      success
    }
  }

  case class RemoveIndexing(product: JsObject) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.removeIndexing(product.value("id").as[String])

    override def nextState(state: State): State = {
      val toReplace = state.indices(state.alias.head).toSeq.find(prod => prod.value("id").as[String] == product.value("id").as[String])
      if (toReplace.isDefined) {
        state.copy(indices = state.indices + (state.alias.head -> (state.indices(state.alias.head) - product)))
      } else {
        state
      }
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get == 200
      if (!success) {
        println("RemoveIndexing")
        println("  " + result.get)
      }
      success
    }
  }

  case class GetProductIndex(index: String, product: JsObject) extends Command {

    override type Result = JsObject

    override def run(sut: Sut): Result = { sut.getProductIndex(index, product.value("id").toString()) }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val updated_result = result.get - "isInStock"
      val success = sortJs(product).toString() == sortJs(updated_result).toString()
      if (!success) {
        println("GetProductIndex")
        println("  " + result.get)
      }
      success
    }
  }

  case class DeleteProductIndex(index: String, product: JsObject) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = { sut.deleteProductIndex(index, product.value("id").toString()) }

    override def nextState(state: State): State = {
      val toReplace = state.indices(index).toSeq.find(prod => prod.value("id").as[String] == product.value("id").as[String])
      if (toReplace.isDefined) {
        state.copy(indices = state.indices + (index -> (state.indices(index) - product)))
      } else {
        state
      }
    }

    override def preCondition(state: State): Boolean = state.alias.nonEmpty

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get == 200
      if (!success) {
        println("RemoveIndexing")
        println("  " + result.get)
      }
      success
    }
  }

  def sortJs(js: JsValue): JsValue = js match {
    case JsObject(fields) => JsObject(fields.toSeq.sortBy(_._1).map { case (key, value) => (key, sortJs(value.asInstanceOf[JsValue])) })
    case JsArray(array) => JsArray(array.map(e => sortJs(e)))
    case other => other
  }
}

object Runner extends Properties("Hesehus") {

  println("Initial reset")
  new HesehusApi().reset()

  var testRuns = 0

  property("HesehusCommands") = {
    testRuns += 1
    println("\nTest run number " + testRuns)
    HesehusSpecification.property()
  }

}

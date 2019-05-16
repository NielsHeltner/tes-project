import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

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
    def genInitialIndices: Gen[List[String]] = for {
      size <- Gen.choose(0, 10)
    } yield 0.to(size).foldLeft(List[String]())((acc, _) => acc :+ new HesehusApi().createIndex._1)

    def genInitialAlias(indices: List[String]): Gen[List[String]] = {
      Gen.someOf(indices).map(List[String])
    }

    for {
      indices <- genInitialIndices
      alias <- genInitialAlias(indices)
    } yield Model(indices = indices, alias = alias)
  }

  def genPutAlias(state: State): Gen[PutAlias] = {
    if (state.alias.isEmpty) { //if alias is already empty, and PutAlias with empty list, then code 500
      Gen.atLeastOne(state.indices).map(PutAlias)
    }
    else {
      Gen.someOf(state.indices).map(PutAlias)
    }
  }

  def genRemoveIndex(state: State): Gen[RemoveIndex] = {
    Gen.oneOf(state.indices).map(RemoveIndex)
  }

  def genCreateIndexing(state: State): Gen[CreateIndexing] = {
    val body = Json.parse(getClass.getResourceAsStream("postIndexingBody.json")).as[JsObject]
    for {
      json <- JsonGenerator.genJson(body)
    } yield CreateIndexing(json)
  }

  def genGetIndexing(state: State): Gen[GetIndexing] = {
    Gen.oneOf(state.products).map(GetIndexing)
  }

  def genPutIndexing(state: State): Gen[PutIndexing] = {
    val body = Json.parse(getClass.getResourceAsStream("postIndexingBody.json")).as[JsObject]
    for {
      json <- JsonGenerator.genJson(body)
      product <- Gen.oneOf(state.products)
    } yield PutIndexing(json ++ Json.obj("id" -> product.value("id")))
  }

  def genRemoveIndexing(state: State): Gen[RemoveIndexing] = {
    Gen.oneOf(state.products).map(RemoveIndexing)
  }

  /** A generator that, given the current abstract state, should produce a suitable Command instance.
    */
  override def genCommand(state: State): Gen[Command] = {
    if (state.indices.isEmpty) {
      Gen.frequency(
        (10, CreateIndex()),
        (5, GetIndices()),
        (5, GetAlias()),
        (5, genCreateIndexing(state))
      )
    } else {
      Gen.frequency(
        (10, CreateIndex()),
        (5, GetIndices()),
        (5, GetAlias()),
        (5, genRemoveIndex(state)),
        (5, genPutAlias(state)),
        (5, genCreateIndexing(state))
      )
    }
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
        state.copy(indices = state.indices :+ response._1)
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

    override def run(sut: Sut): Result = sut.getIndices

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = state.indices.sorted == result.get.sorted
      if (!success) {
        println("GetIndices")
        println("  State: " + state.indices)
        println("  API: " + result.get)
      }
      success
    }
  }

  case class RemoveIndex(index: String) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.removeIndex(index)

    override def nextState(state: State): State = state.copy(indices = state.indices.filterNot(_ == index), alias = state.alias.filterNot(_ == index))

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
      val success = state.alias.sorted == result.get.sorted
      if (!success) {
        println("GetAlias")
        println("  State: " + state.alias)
        println("  API: " + result.get)
      }
      success
    }
  }

  case class PutAlias(indices: Seq[String]) extends Command {

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

  case class CreateIndexing(product: JsObject) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.createIndexing(product)

    override def nextState(state: State): State = {
      state.copy(products = state.products :+ product)
    }

    override def preCondition(state: State): Boolean = true

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

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val updated_result = result.get - "isInStock"
      val success = sortJs(product).toString() == sortJs(updated_result).toString()
      if (!success) {
        println("GetIndexing")
        println("  " + Json.prettyPrint(result.get))
      }
      success
    }

    def sortJs(js: JsValue): JsValue = js match {
      case JsObject(fields) => JsObject(fields.toSeq.sortBy(_._1).map { case (key, value) => (key, sortJs(value.asInstanceOf[JsValue])) })
      case JsArray(array) => JsArray(array.map(e => sortJs(e)))
      case other => other
    }
  }

  case class PutIndexing(product: JsObject) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.putIndexing(product)

    override def nextState(state: State): State = {
      val toReplace = state.products.find(prod => prod.value("id").as[String] == product.value("id").as[String])
      if (toReplace.isDefined) {
        state.copy(products = state.products.filterNot(_ == toReplace.get) :+ product)
      } else {
        state
      }
    }

    override def preCondition(state: State): Boolean = true

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
      val toReplace = state.products.find(prod => prod.value("id").as[String] == product.value("id").as[String])
      if (toReplace.isDefined) {
        state.copy(products = state.products.filterNot(_ == toReplace.get))
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
}

object Runner extends Properties("Hesehus") {

  println("Initial reset")
  new HesehusApi().reset()

  var testRuns = 0

  property("HesehusCommands") = {
    testRuns += 1
    println("Test run number " + testRuns)
    HesehusSpecification.property()
  }

}

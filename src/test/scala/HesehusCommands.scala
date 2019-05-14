import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import play.api.libs.json.{JsObject, JsValue, Json}

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

    def genInitialIndices(): Gen[List[String]] = {
      0.to(Gen.choose(0, 10).sample.get).foldLeft(List[String]())((acc, _) => acc :+ new HesehusApi().createIndex._1)
    }

    def genInitialAlias(indices: List[String]): Gen[String] = {
      Gen.oneOf(indices)
    }

    def genInitialProduct: JsObject = {
      val body = Json.parse(getClass.getResourceAsStream("postIndexingBody.json")).as[JsObject]
      val generatedJson = JsonGenerator.parseJs(body).as[JsObject]

      new HesehusApi().postIndexing(generatedJson)
      generatedJson.as[JsObject]
    }

    val indices = genInitialIndices().sample.get
    val alias = Seq[String](genInitialAlias(indices).sample.get)
    val products = Seq[JsObject](genInitialProduct)
    Gen.const(Model(indices = indices, alias = alias, products = products))
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

  def genSearch(state: State): Gen[PostSearch] =  {
    val body = Json.parse(getClass.getResourceAsStream("searchAllProductsBody.json")).as[JsObject]
    val generatedJson = JsonGenerator.parseJs(body).as[JsObject]
    PostSearch(generatedJson)
  }

  /** A generator that, given the current abstract state, should produce a suitable Command instance.
    */
  override def genCommand(state: State): Gen[Command] = {
    if (state.indices.isEmpty) {
      Gen.frequency(
        (10, CreateIndex()),
        (5, GetIndices()),
        (5, GetAlias())
      )
    }
    else {
      Gen.frequency(
        (10, CreateIndex()),
        (5, GetIndices()),
        (5, GetAlias()),
        (5, genRemoveIndex(state)),
        //(5, genPutAlias(state)),
        (10, genSearch(state))
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

  case class PostSearch(generatedJson: JsObject) extends Command {

    override type Result = List[String]

    override def run(sut: Sut): Result = sut.postSearch(generatedJson)

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val searchFilter = new SearchFilter
      val filteredProducts = searchFilter.filter(generatedJson, state.products)
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

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import scalaj.http.HttpResponse

import scala.collection.immutable
import scala.collection.immutable.HashMap
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
    def genInitialIndices: Gen[HashMap[String, Seq[JsObject]]] = for {
      size <- Gen.choose(1, 10)
    } yield 1.to(size).foldLeft(HashMap[String, Seq[JsObject]]())((acc, _) => acc + (new HesehusApi().createIndex._1 -> Seq[JsObject]()))

    def genInitialAlias(indices: HashMap[String, Seq[JsObject]]): Gen[List[String]] = {
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

  def genSearch(state: State): Gen[Search] = for {
    json <- JsonGen.genSearchJson
  } yield Search(json)

  def genCreateIndexing(state: State): Gen[CreateIndexing] = for {
    json <- JsonGen.genProductJson()
  } yield CreateIndexing(json)

  def genGetIndexing(state: State): Gen[GetIndexing] = {
    Gen.oneOf(state.currentIndices).map(GetIndexing)
  }

  def genPutIndexing(state: State): Gen[PutIndexing] = for {
    json <- JsonGen.genProductJson()
    product <- Gen.oneOf(state.currentIndices)
  } yield PutIndexing(json ++ Json.obj("id" -> product.value("id")))

  def genRemoveIndexing(state: State): Gen[RemoveIndexing] = {
    Gen.oneOf(state.currentIndices).map(RemoveIndexing)
  }

  def genPostProductIndex(state: State): Gen[PostProductIndex] = for {
    index <- Gen.oneOf(state.indices.keys.toSeq)
    product <- JsonGen.genProductJson()
  } yield PostProductIndex(index, product)

  def genGetProductIndex(state: State): Gen[GetProductIndex] = for {
    index <- Gen.oneOf(state.indicesWithProducts)
    product <- Gen.oneOf(state.indices(index))
  } yield GetProductIndex(index, product)

  def genDeleteProductIndex(state: State): Gen[DeleteProductIndex] = for {
    index <- Gen.oneOf(state.indicesWithProducts)
    product <- Gen.oneOf(state.indices(index))
  } yield DeleteProductIndex(index, product)


  def genUpsertBulk(state: State): Gen[UpsertBulk] = for {
    index <- Gen.oneOf(state.indicesWithProducts)
    product <- Gen.atLeastOne(state.indices(index))
  } yield UpsertBulk(index, product)

  def genGetBulk(state: State): Gen[GetBulk] = for {
    index <- Gen.oneOf(state.indicesWithProducts)
    product <- Gen.atLeastOne(state.indices(index))
  } yield GetBulk(index, product)

  def genDeleteBulk(state: State): Gen[DeleteBulk] = for {
    index <- Gen.oneOf(state.indicesWithProducts)
    product <- Gen.atLeastOne(state.indices(index))
  } yield DeleteBulk(index, product)


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
        genPutAlias(state),
        genPostProductIndex(state)
      )
      if (state.containsProducts) {
        cmds = cmds ++ Seq[Gen[Command]](
          genGetProductIndex(state),
          genDeleteProductIndex(state),
          genGetBulk(state),
          genDeleteBulk(state),
          genUpsertBulk(state)
        )
      }
    }
    if (state.alias.nonEmpty) {
      cmds = cmds ++ Seq[Gen[Command]](
        genCreateIndexing(state),
        genSearch(state)
      )
      if (state.aliasContainsProducts) {
        cmds = cmds ++ Seq[Gen[Command]](
          genGetIndexing(state),
          genPutIndexing(state),
          genRemoveIndexing(state)
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
        state.copy(indices = state.indices + (response._1 -> Seq[JsObject]()))
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

  case class CreateIndexing(product: JsObject) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = {
      println(s"Created indexing ${product.value("id")}")
      sut.createIndexing(product)
    }

    override def nextState(state: State): State = {
      state.copy(indices = state.indices + (state.alias.head -> (state.currentIndices :+ product)))
    }

    override def preCondition(state: State): Boolean = state.alias.nonEmpty

    override def postCondition(state: State, result: Try[Result]): Prop = {
      println("CreateIndexing ")
      val success = result.get.code == 200
      if (!success) {
        println("CreateIndexing")
        //println("  " + result.get)
        println("Alias: " + state.alias + s" size (${state.currentIndices.size})")
        println(Json.prettyPrint(product))
        if (result.get.body.nonEmpty)
          println(Json.prettyPrint(Json.parse(result.get.body)))
      }
      success
    }
  }

  case class GetIndexing(product: JsObject) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = sut.getIndexing(product.value("id").as[String])

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = state.aliasContainsProducts

    override def postCondition(state: State, result: Try[Result]): Prop = {
      println("GetIndexing")
      if (result.get.code != 200) {
        println("GetIndexing")
        println(result.get.code)
        if (result.get.body.nonEmpty)
          println(Json.prettyPrint(Json.parse(result.get.body)))
        false
      }
      else {
        val updatedResult = Json.parse(result.get.body).as[JsObject] - "isInStock"
        val succ1 = product.value.size == updatedResult.value.size
        if (!succ1) {
          println(s"same size: $succ1")
          println(s"  state size: ${product.value.size}")
          println(s"  api size: ${updatedResult.value.size}")
          if (updatedResult.value.size == 2) {
            updatedResult.value.keys.foreach(println(_))
          }
        }
        //val succ2 = product.value.keys.forall(key => product.value(key) == updatedResult.value(key))
        val succ2 = product.value("id") == updatedResult.value("id")
        val success = succ1 && succ2
        if (!success) {
          println("GetIndexing")
          println(s"same keys? $succ2")
          product.value.keys.filter(key => product.value(key) != updatedResult.value(key)).foreach(key => println(s"  Key: $key\n  API:   ${updatedResult.value(key)}\n  State: ${product.value(key)}"))
          println("Alias: " + state.alias + s" size (${state.currentIndices.size})")
          println("  API:   " + Json.prettyPrint(updatedResult))
          println("  State: " + Json.prettyPrint(product))
        }
        success
      }
    }
  }

  case class PutIndexing(product: JsObject) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = sut.putIndexing(product)

    override def nextState(state: State): State = {
      state.copy(indices = state.indices + (state.alias.head -> (state.currentIndices.filterNot(indexProduct => indexProduct.value("id") == product.value("id")) :+ product)))
    }

    override def preCondition(state: State): Boolean = state.aliasContainsProducts

    override def postCondition(state: State, result: Try[Result]): Prop = {
      println("PutIndexing")
      val success = result.get.code == 200
      if (!success) {
        println("PutIndexing")
        println("Alias: " + state.alias + s" size (${state.currentIndices.size})")
        //println("  " + result.get)
        println(Json.prettyPrint(product))
        if (result.get.body.nonEmpty)
          println(Json.prettyPrint(Json.parse(result.get.body)))
        println(s"  State (size ${state.currentIndices.size}): ")
        state.currentIndices.map(_.value("id")).foreach(println(_))
      }
      success
    }
  }

  case class RemoveIndexing(product: JsObject) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = sut.removeIndexing(product.value("id").as[String])

    override def nextState(state: State): State = {
      state.copy(indices = state.indices + (state.alias.head -> state.currentIndices.filterNot(indexProduct => indexProduct.value("id") == product.value("id"))))
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      println("RemoveIndexing")
      val success = result.get.code == 200
      if (!success) {
        println("RemoveIndexing")
        println(result.get.code)
        println("Alias: " + state.alias + s" size (${state.currentIndices.size})")
        //println("  " + result.get)
        println(Json.prettyPrint(product))
        if (result.get.body.nonEmpty)
          println(Json.prettyPrint(Json.parse(result.get.body)))
        println("product id " + product.value("id"))
      }
      success
    }
  }

  case class PostProductIndex(index: String, product: JsObject) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = {
      println(s"Created product ${product.value("id")} on $index")
      sut.postProductIndex(index, product)
    }

    override def nextState(state: State): State = {
      state.copy(indices = state.indices + (index -> (state.indices(index).filterNot(indexProduct => indexProduct.value("id") == product.value("id")) :+ product)))
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get.code == 200
      if (!success) {
        println("Post Product Index")
        //println("  " + result.get)
        println(Json.prettyPrint(product))
        if (result.get.body.nonEmpty)
          println(Json.prettyPrint(Json.parse(result.get.body)))
      }
      success
    }
  }

  case class GetProductIndex(index: String, product: JsObject) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = {
      sut.getProductIndex(index, product.value("id").as[String])
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.get.code != 200) {
        println("GetIndexing")
        println(Json.prettyPrint(Json.parse(result.get.body)))
        false
      }
      else {
        val updatedResult = Json.parse(result.get.body).as[JsObject] - "isInStock"
        val succ1 = product.value.size == updatedResult.value.size
        if (!succ1) {
          println(s"same size: $succ1")
          println(s"  state size: ${product.value.size}")
          println(s"  api size: ${updatedResult.value.size}")
          if (updatedResult.value.size == 2) {
            updatedResult.value.keys.foreach(println(_))
          }
        }
        //val succ2 = product.value.keys.forall(key => product.value(key) == updatedResult.value(key))
        val succ2 = product.value("id") == updatedResult.value("id")
        val success = succ1 && succ2
        if (!success) {
          println("GetIndexing")
          println(s"same keys? $succ2")
          product.value.keys.filter(key => product.value(key) != updatedResult.value(key)).foreach(key => println(s"  Key: $key\n  API:   ${updatedResult.value(key)}\n  State: ${product.value(key)}"))
          println("Alias: " + state.alias)
          println("  API:   " + Json.prettyPrint(updatedResult))
          println("  State: " + Json.prettyPrint(product))
        }
        success
      }
    }
  }

  case class DeleteProductIndex(index: String, product: JsObject) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = {
      sut.deleteProductIndex(index, product.value("id").as[String])
    }

    override def nextState(state: State): State = state.copy(indices = state.indices + (index -> state.indices(index).filterNot(_ == product)))

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get.code == 200
      if (!success) {
        println("DeleteProductIndex")
        println(Json.prettyPrint(product))
        if (result.get.body.nonEmpty)
          println(Json.prettyPrint(Json.parse(result.get.body)))
        println("product id " + product.value("id"))
      }
      success
    }
  }

  case class UpsertBulk(index: String, products: Seq[JsObject]) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = {
      sut.upsertBulk(index, products)
    }

    override def nextState(state: State): State = {
      val productIdsToDelete = products.map(jsObject => jsObject.value("id").as[String])
      state.copy(indices = state.indices + (index -> state.indices(index).filterNot(product => productIdsToDelete.contains(product.value("id").as[String])).++(products)))
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get.code == 200
      if (!success) {
        println("Upsert Bulk")
        //println(Json.prettyPrint(products))
        if (result.get.body.nonEmpty)
          println(Json.prettyPrint(Json.parse(result.get.body)))
        //println("product id " + product.value("id"))
      }
      success
    }
  }

  case class GetBulk(index: String, products: Seq[JsObject]) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = {
      sut.getBulk(index, products.map(jsObject => jsObject.value("id").as[String]))
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.get.code != 200) {
        println("Get bulk")
        println(Json.prettyPrint(Json.parse(result.get.body)))
        false
      }
      else {
        val updatedResults = Json.parse(result.get.body).as[List[JsObject]]
        updatedResults.foreach(product => product - "isInStock")

        val succ1 = products.size == updatedResults.size
        if (!succ1) {
          println(s"same size: $succ1")
          println(s"  state size: ${products.size}")
          println(s"  api size: ${updatedResults.size}")
          if (updatedResults.size == 2) {
            updatedResults.foreach(println(_))
          }
        }
        //val succ2 = product.value.keys.forall(key => product.value(key) == updatedResult.value(key))
        val succ2 = products.map(product => product.value("id").as[String]).sorted == updatedResults.map(product => product.value("id").as[String]).sorted
        val success = succ1 && succ2
        if (!success) {
          println("GetIndexing")
          /*println(s"same keys? $succ2")
          product.value.keys.filter(key => product.value(key) != updatedResult.value(key)).foreach(key => println(s"  Key: $key\n  API:   ${updatedResult.value(key)}\n  State: ${product.value(key)}"))
          println("Alias: " + state.alias)
          println("  API:   " + Json.prettyPrint(updatedResults))
          println("  State: " + Json.prettyPrint(product))*/
        }
        success
      }
    }
  }

  case class DeleteBulk(index: String, products: Seq[JsObject]) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = {
      sut.deleteBulk(index, products.map(jsObject => jsObject.value("id").as[String]))
    }

    override def nextState(state: State): State = {
      val productIdsToDelete = products.map(jsObject => jsObject.value("id").as[String])

      state.copy(indices = state.indices + (index -> state.indices(index).filterNot(product => productIdsToDelete.contains(product.value("id").as[String]))))
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val success = result.get.code == 200
      if (!success) {
        println("Delete Bulk")
        //println(Json.prettyPrint(products))
        if (result.get.body.nonEmpty)
          println(Json.prettyPrint(Json.parse(result.get.body)))
        //println("product id " + product.value("id"))
      }
      success
    }
  }

  case class Search(generatedJson: JsObject) extends Command {

    override type Result = HttpResponse[String]

    override def run(sut: Sut): Result = {
      sut.search(generatedJson)
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      println("Search")
      val searchFilter = new SearchFilter
      val filteredProducts = searchFilter.filter(generatedJson, state.currentIndices.toSeq)
      val returnedProducts = Json.parse(result.get.body).as[JsObject].value("productResults").as[List[JsObject]].map(jsObject => jsObject.value("product").as[JsObject])
      val returnedIdsSorted = returnedProducts.map(product => product.value("id").as[String]).sorted

      val success = filteredProducts.size == returnedIdsSorted.size &&
        filteredProducts.indices.count(index => filteredProducts(index).as[JsObject].value("id").as[String] != returnedIdsSorted(index)) == 0
      if (!success) {
        println("PostSearch")
        println("  Current alias: " + state.alias.head)
        println("  API counted size: " + returnedIdsSorted.size)
        println("  API size: " + Json.parse(result.get.body).as[JsObject].value("totalProductResultsFound").as[Int])
        println("  State size: " + filteredProducts.size)
        println("  State size in general: " + state.currentIndices.size)
        println("  Filter: " + Json.prettyPrint(generatedJson))
        println("  State: " + filteredProducts)
        println("  API: " + Json.parse(result.get.body).as[JsObject])
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
    new HesehusApi().reset()
    HesehusSpecification.property()
  }

}

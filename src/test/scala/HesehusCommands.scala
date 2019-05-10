import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import play.api.libs.json.{JsArray, JsObject}

import scala.util.{Random, Success, Try}

//https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md#stateful-testing
//https://github.com/rickynils/scalacheck/tree/master/examples/commands-nix
object HesehusSpecification extends Commands {

  override type State = Model

  override type Sut = HesehusApi

  /** Decides if [[newSut]] should be allowed to be called with the specified state instance. This can be used to limit
    * the number of co-existing [[Sut]] instances. The list of existing states represents the initial states (not the
    * current states) for all [[Sut]] instances that are active for the moment. If this method is implemented incorrectly,
    * for example if it returns false even if the list of existing states is empty, ScalaCheck might hang.
    *
    * If you want to allow only one [[Sut]] instance to exist at any given time (a singleton [[Sut]]), implement this
    * method the following way:
    *
    *  {{{
    *  def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[Sut]) = {
    *    initSuts.isEmpty && runningSuts.isEmpty
    *  }
    *  }}}
    */
  override def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[Sut]): Boolean = {
    true
  }

  /** The precondition for the initial state, when no commands yet have run. This is used by ScalaCheck when command
    * sequences are shrunk and the first state might differ from what is returned from [[genInitialState]].
    **/
  override def initialPreCondition(state: State): Boolean = {
    state.isEmpty
  }

  /** Create a new [[Sut]] instance with an internal state that corresponds to the provided abstract state instance.
    * The provided state is guaranteed to fulfill [[initialPreCondition]], and [[newSut]] will never be called if
    * [[canCreateNewSut]] is not true for the given state.
    */
  override def newSut(state: State): Sut = {
    //println("New Sut")
    //println("Test run #" + testNumber + " of 100") //TODO: Hvis muligt, hent antal fra props i stedet for hardcoded.

    //testNumber = testNumber + 1
    new HesehusApi
  }

  /** Destroy the system represented by the given [[Sut]] instance, and release any resources related to it.
    */
  override def destroySut(sut: Sut): Unit = CleanUp.cleanUp

  /** A generator that should produce an initial [[State]] instance that is usable by [[newSut]] to create a new system
    * under test. The state returned by this generator is always checked with the [[initialPreCondition]] method before
    * it is used.
    */
  override def genInitialState: Gen[State] = {
    Gen.const(new Model)
  }

  def genPutAlias(state: State): Gen[PutAlias] = {
    Gen.someOf(state.indices).map(PutAlias)
  }

  /** A generator that, given the current abstract state, should produce a suitable Command instance.
    */
  override def genCommand(state: State): Gen[Command] = {

    //println(state.getIndices)
    Gen.frequency(
      (10, CreateIndex()),
      (10, GetIndices)
      //(10, RemoveIndex()),
      //(10, Gen.const(GetAlias)),
      //(10, genPutAlias(state))
      //(5, genRemoveIndex(state))
    )
  }

  /*def genRemoveIndex(state: State): Gen[RemoveIndex] = {
    val index = if(state.containIndices) state.getIndex(Random.nextInt(state.indices.size)) else ""
    RemoveIndex(index)
  } //yield RemoveIndex(index)
*/

  /*def genRemoveIndex(state: State): Gen[RemoveIndex] = {
    println("genCommand state " + state)
    if(state.containIndices) {
      println("Contain")
      Gen.oneOf(state.indices).map(RemoveIndex)
    } else {
      println("Not Contain")
      Gen.identifier.map(RemoveIndex)
    }
  }*/

  case class CreateIndex() extends Command {

    var response: (String, Boolean) = _
    val i = Random.nextInt(10000)

    override type Result = Boolean

    override def run(sut: Sut): Result = {
      println("CreateIndex " + i)

      response._2
    }

    override def nextState(state: State): State = {
      //println("nextState CreateIndex " + i)
      //state.addIndex("Test")
      //state.addIndex(response._1)
      response = new HesehusApi().createIndex

      //update state
      //state.addIndex()
      //println("Run state " + state)
      var seq: Seq[String] = state.indices :+ response._1
      //val s = state.clone(indices = (state.indices + response._1))
      var s = state.clone(indices = seq)
      println("State " + seq)
      s
    }

    override def preCondition(state: State): Boolean = {
      //println("preCondition CreateIndex")
      true
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      //println("postCondition CreateIndex " + i)
      // print("Response: " + response._1)
      //this.state.addIndex(response._1)
      //println("This should be true: " + state.containIndices)
      //println("Postcon state " + state)
      result == Success(true)
    }
  }

  case object GetIndices extends Command {
    override type Result = List[String]

    override def run(sut: Sut): Result = {
      println("GetIndices")

      //println(sut.getIndices)
      sut.getIndices
    }

    override def nextState(state: State): State = {
      //println("nextState GetIndices")
      //println("NextState: " + state.getIndices.sorted)

      state
    }

    override def preCondition(state: State): Boolean = {
      //println("preCondition GetIndices")

      true//state.containIndices
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      //println("postCondition GetIndices")

      println("State: " + state.getIndices.sorted)
      println("Sut: " + result.get.sorted)
      println("Dunno: " + state.getIndices.sorted.equals(result.get.sorted))
      //(SortedSet[String]() ++ state.getIndices.toSet).sameElements(SortedSet[String]() ++ result.get.toSet)
      state.getIndices.sorted.equals(result.get.sorted)
    }
  }

  case class RemoveIndex() extends Command {

    var index: String = _

    override type Result = Boolean

    override def run(sut: Sut): Result = {
      //println("RemoveIndex")
      sut.removeIndex(index)
    }

    override def nextState(state: State): State = {
      println("Remove State: " + state.getIndices.sorted)
      //println("Sut: " + result.get.sorted)

      index = if(state.containIndices) state.getIndex(Random.nextInt(state.indices.size)) else ""
      println("Index to be removed: " + index)
      state.removeIndex(index)
    }

    override def preCondition(state: State): Boolean = true//state.containIndices

    override def postCondition(state: State, result: Try[Result]): Prop = {
      result == Success(state.getIndices.contains(index))
    }
  }

  case object GetAlias extends Command {

    override type Result = JsArray

    override def run(sut: Sut): Result = sut.getAlias

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val ids = result.get.value.map(_.as[JsObject]).map(_.value("id").as[String])
      state.alias.indices.count { i => ids(i) != state.alias(i) } == 0
    }

  }

  case class PutAlias(indices: Seq[String]) extends UnitCommand {

    override def run(sut: Sut): Unit = {
      sut.putAlias(indices)
    }

    override def nextState(state: State): State = {
      state.clone(alias = indices)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, success: Boolean): Prop = success

  }

}

object Runner extends Properties("Hesehus") {

  property("HesehusCommands") = HesehusSpecification.property()

}

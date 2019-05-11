import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.Try

//https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md#stateful-testing
//https://github.com/rickynils/scalacheck/tree/master/examples/commands-nix
object HesehusSpecification extends Commands {

  /** The abstract state type. Must be immutable.
    * The [[State]] type should model the state of the system under
    * test (SUT). It should only contain details needed for specifying
    * our pre- and postconditions, and for creating [[Sut]]
    *  instances. */
  override type State = Model

  override type Sut = HesehusApi

  override def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[Sut]): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  /** The precondition for the initial state, when no commands yet have run. This is used by ScalaCheck when command
    * sequences are shrunk and the first state might differ from what is returned from [[genInitialState]].
    * */
  override def initialPreCondition(state: State): Boolean = true

  /** Create a new [[Sut]] instance with an internal state that
    * corresponds to the provided abstract state instance. The provided state
    * is guaranteed to fulfill [[initialPreCondition]], and
    * [[newSut]] will never be called if
    * [[canCreateNewSut]] is not true for the given state. */
  override def newSut(state: State): Sut = {
    val api = new HesehusApi
    api.putAlias(state.alias)
    api
  }

  /** Destroy the system represented by the given [[Sut]] instance, and release any resources related to it.
    */
  override def destroySut(sut: Sut): Unit = ()//sut.reset()

  /** A generator that should produce an initial [[State]] instance that is usable by [[newSut]] to create a new system
    * under test. The state returned by this generator is always checked with the [[initialPreCondition]] method before
    * it is used.
    */
  override def genInitialState: Gen[State] = {
    val index = Seq[String](new HesehusApi().createIndex._1)
    Gen.const(Model(indices = index))
  }

  def genPutAlias(state: State): Gen[PutAlias] = {
    if (state.alias.isEmpty) { //if alias is already empty, and PutAlias with empty list = code 500
      Gen.atLeastOne(state.indices).map(PutAlias)
    }
    else {
      Gen.someOf(state.indices).map(PutAlias)
    }
  }

  def genRemoveIndex(state: State): Gen[RemoveIndex] = {
    Gen.oneOf(state.indices).map(RemoveIndex)
  }

  /** A generator that, given the current abstract state, should produce a suitable Command instance.
    */
  override def genCommand(state: State): Gen[Command] = {
    //println(state.hashCode())
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
        (5, genPutAlias(state))
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
      if (response._1 != "-1") {
        state.copy(indices = state.indices :+ response._1)
      }
      else {
        state
      }
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val r = result.get == 200
      if (!r) {
        println("CreateIndex")
        println(result.get)
      }
      r
    }
  }

  case class GetIndices() extends Command {

    override type Result = List[String]

    override def run(sut: Sut): Result = sut.getIndices

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val r = state.indices.sorted == result.get.sorted
      if (!r) {
        println("GetIndices")
        println("State: " + state.indices)
        println("API: " + result.get)
      }
      r
    }
  }

  case class RemoveIndex(index: String) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.removeIndex(index)

    override def nextState(state: State): State = state.copy(indices = state.indices.filterNot(_ == index))

    override def preCondition(state: State): Boolean = state.indices.contains(index)

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val r = result.get == 200
      if (!r) {
        println("RemoveIndex")
        println(result.get)
      }
      r
    }
  }

  case class GetAlias() extends Command {

    override type Result = List[String]

    override def run(sut: Sut): Result = sut.getAlias

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val r = state.alias.sorted == result.get.sorted
      if (!r) {
        println("GetAlias")
        println("State: " + state.alias)
        println("API: " + result.get)
      }
      r
    }
  }

  case class PutAlias(indices: Seq[String]) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = {
      sut.putAlias(indices)
    }

    override def nextState(state: State): State = state.copy(alias = indices)

    override def preCondition(state: State): Boolean = indices.forall(state.indices.contains)

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val r = result.get == 200
      if (!r) {
        println("PutAlias")
        println(result.get)
      }
      r
    }
  }

}

object Runner extends Properties("Hesehus") {

  println("init resetttttttttttttttttttttttttttttttttttttttttttttt")
  new HesehusApi().reset()

  property("HesehusCommands") = {
    new HesehusApi().reset()
    HesehusSpecification.property()
  }

}

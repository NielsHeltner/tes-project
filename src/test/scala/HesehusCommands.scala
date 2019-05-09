import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.{Success, Try}

//https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md#stateful-testing
//https://github.com/rickynils/scalacheck/tree/master/examples/commands-nix
object HesehusSpecification extends Commands {

  var testNumber = 1

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
    println("Test run #" + testNumber + " of 100") //TODO: Hvis muligt, hent antal fra props i stedet for hardcoded.

    testNumber = testNumber + 1
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
    //println("New model")

    new Model
  }

  /** A generator that, given the current abstract state, should produce a suitable Command instance.
    */
  override def genCommand(state: State): Gen[Command] = Gen.frequency(
    (1, CreateIndex()),
    (10, GetIndices)
  )

  case class CreateIndex() extends Command {

    var response: (String, Boolean) = _

    override type Result = Boolean

    override def run(sut: Sut): Result = {
      //println("CreateIndex")

      response = sut.createIndex
      //response._1
      response._2
    }

    override def nextState(state: State): State = {
      //println("nextState CreateIndex")
      state//state.addIndex("test")
    }

    override def preCondition(state: State): Boolean = {
      //println("preCondition CreateIndex")
      true
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      //println("postCondition CreateIndex")
      //print("Response: " + response._1)
      state.addIndex(response._1)
      //println("This should be true: " + state.containIndices)
      result == Success(true)
    }
  }

  case object GetIndices extends Command {
    override type Result = List[String]

    override def run(sut: Sut): Result = {
      //println("GetIndices")

      //println(sut.getIndices)
      sut.getIndices
    }

    override def nextState(state: State): State = {
      //println("nextState GetIndices")

      state
    }

    override def preCondition(state: State): Boolean = {
      //println("preCondition GetIndices")

      true//state.containIndices
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      //println("postCondition GetIndices")

      /*println("State: " + state.getIndices.sorted)
      println("Sut: " + result.get.sorted)
      println("Dunno: " + state.getIndices.sorted.equals(result.get.sorted))*/
      //(SortedSet[String]() ++ state.getIndices.toSet).sameElements(SortedSet[String]() ++ result.get.toSet)
      state.getIndices.sorted.equals(result.get.sorted)
    }
  }

  case object GetAmount extends Command {

    override type Result = String

    override def run(sut: Sut): Result = {

      sut.getAmount
    }

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {

      println("TEST")

      result == Success(state.getAmount)
    }

  }

}

object Runner extends Properties("Hesehus") {

  property("HesehusCommands") = HesehusSpecification.property()

}

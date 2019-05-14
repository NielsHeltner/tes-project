import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Success, Try}

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
    val api = new HesehusApi
    api.putAlias(state.alias)
    api
  }

  /** Destroy the system represented by the given [[Sut]] instance, and release any resources related to it.
    */
  override def destroySut(sut: Sut): Unit = ()

  /** A generator that should produce an initial [[State]] instance that is usable by [[newSut]] to create a new system
    * under test. The state returned by this generator is always checked with the [[initialPreCondition]] method before
    * it is used.
    */
  override def genInitialState: Gen[State] = {
    Gen.const(Model())
  }

  def genPutAlias(state: State): Gen[PutAlias] = {
    if (state.alias.isEmpty) { //code 500 if putalias with empty list, if alias is already empty
      Gen.atLeastOne(state.indices).map(PutAlias)
    }
    else {
      Gen.someOf(state.indices).map(PutAlias)
    }
  }

  /** A generator that, given the current abstract state, should produce a suitable Command instance.
    */
  override def genCommand(state: State): Gen[Command] = {
    Gen.oneOf(
      Gen.const(GetAlias()),
      genPutAlias(state)
    )
  }

  case class GetAlias() extends Command {

    override type Result = JsArray

    override def run(sut: Sut): Result = sut.getAlias

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      val ids = result.get.value.map(_.as[JsObject]).map(_.value("id").as[String])
      state.alias == ids
    }

  }

  case class PutAlias(indices: Seq[String]) extends Command {

    override type Result = Int

    override def run(sut: Sut): Result = sut.putAlias(indices).code

    override def nextState(state: State): State = state.copy(alias = indices)

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = result.get == 200

  }

}

object Runner extends Properties("Hesehus") {

  property("HesehusCommands") = HesehusSpecification.property()

}

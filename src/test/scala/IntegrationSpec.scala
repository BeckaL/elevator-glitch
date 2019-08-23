import org.scalatest.{FlatSpec, Matchers}

class IntegrationSpec extends FlatSpec with Matchers with CompletePrintedJourney {

  "A simulator" should "convert an elevator state to a state to render" in {
    val state = ElevatorState(List(Person(1, 2, 0)), List(Lift(0, None, List(), "")), List(), 0, List())
    val r = SceneRenderer

    val sceneToPrint = r.convertStateToScene(state)
    sceneToPrint shouldBe SceneToRender(LiftToRender(0, 0, ""), List(PersonOnFloor(1)), List())

    val printedScene = r.createScene(sceneToPrint)
    printedScene shouldBe stateOne
  }

  "A simulator" should "convert a second elevator state to a state to render" in {
    val state = ElevatorState(List(Person(1, 2, 0)), List(Lift(0, None, List(), "left")), List(), 0, List())
    val r = SceneRenderer

    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)
    printedScene shouldBe stateTwo
  }

  "A simulator" should "convert a third elevator state to a state to render" in {
    val state = ElevatorState(List(), List(Lift(0, None, List(Person(1, 2, 0)), "left")), List(), 0, List())
    val r = SceneRenderer

    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)
    printedScene shouldBe stateThree
  }


  "A simulator" should "convert a fourth elevator state to a state to render" in {
    val state = ElevatorState(List(), List(Lift(0, None, List(Person(1, 2, 0)), "")), List(), 0, List())
    val r = SceneRenderer

    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)
    printedScene shouldBe stateFour
  }

  "A simulator" should "convert a fifth elevator state to a state to render" in {
    val state = ElevatorState(List(), List(Lift(1, None, List(Person(1, 2, 0)), "")), List(), 0, List())
    val r = SceneRenderer

    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)
    printedScene shouldBe stateFive
  }
}

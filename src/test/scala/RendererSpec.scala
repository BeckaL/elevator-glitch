import org.scalatest.{FlatSpec, Matchers}

class RendererSpec extends FlatSpec with Matchers with CompletePrintedJourney {
  val r = SceneRenderer

  "A simulator" should "convert an elevator state to a state to render" in {
    val state = ElevatorState(List(Person(0, 1, 0)), List(Lift(LiftLocation(0, 0), None, List(), "")), List(), 0, List())

    val sceneToPrint = r.convertStateToScene(state)
    sceneToPrint shouldBe SceneToRender(LiftToRender(0, 0, ""), List(PersonOnFloor(0)), List())

    val printedScene = r.createScene(sceneToPrint)
    printedScene shouldBe states(0)
  }

  "A simulator" should "convert a second elevator state to a state to render" in {
    val state = ElevatorState(List(Person(0, 1, 0)), List(Lift(LiftLocation(0, 0), None, List(), "left")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe states(1)
  }

  "A simulator" should "convert a third elevator state to a state to render" in {
    val state = ElevatorState(List(), List(Lift(LiftLocation(0, 0), None, List(Person(1, 2, 0)), "left")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe states(2)
  }


  "A simulator" should "convert a fourth elevator state to a state to render" in {
    val state = ElevatorState(List(), List(Lift(LiftLocation(0, 0), None, List(Person(1, 2, 0)), "")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe states(3)
  }

  "A simulator" should "convert a fifth elevator state to a state to render" in {
    val state = ElevatorState(List(), List(Lift(LiftLocation(0, 1), None, List(Person(1, 2, 0)), "")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe states(4)
  }
}

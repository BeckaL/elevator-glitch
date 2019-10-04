import elevatorGlitch.Renderer.SceneRenderer
import elevatorGlitch.Renderer.SceneRenderer.{LiftToRender, PersonOnFloor, SceneToRender}
import elevatorGlitch.Simulator.{ElevatorState, Lift, LiftLocation, Person}
import org.scalatest.{FlatSpec, Matchers}

class RendererSpec extends FlatSpec with Matchers with CompletePrintedJourney with IndividualScenes {
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

  "A simulator" should "should show two people in a lift" in {
    val state = ElevatorState(List(), List(Lift(LiftLocation(0, 0), None, List(Person(1, 2, 0), Person(1, 2, 0)), "")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe twoPeopleInOneLift
  }

  "A simulator" should "should show three people in a lift" in {
    val state = ElevatorState(List(), List(Lift(LiftLocation(0, 0), None, List(Person(1, 2, 0), Person(1, 2, 0),  Person(1, 2, 0)), "")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe threePeopleInOneLift
  }

  "A simulator" should "should show two people in a lift with a left door open" in {
    val state = ElevatorState(List(), List(Lift(LiftLocation(0, 0), None, List(Person(1, 2, 0), Person(1, 2, 0)), "left")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe twoPeopleInOneLiftWithLeftDoorOpen
  }

  "A simulator" should "should show two people in a lift with a right door open" in {
    val state = ElevatorState(List(), List(Lift(LiftLocation(0, 0), None, List(Person(1, 2, 0), Person(1, 2, 0)), "right")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe twoPeopleInOneLiftWithRightDoorOpen
  }

  "A simulator" should "should show two people waiting" in {
    val state = ElevatorState(List(Person(0, 2, 0), Person(0, 2, 0)), List(Lift(LiftLocation(0, 0), None, List(), "")), List(), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    printedScene shouldBe twoPeopleWaitingOnGroundFloor
  }

  "A simulator" should "should show two people exiting" in {
    val state = ElevatorState(List(), List(Lift(LiftLocation(0, 0), None, List(), "")), List(Person(1, 0, 0), Person(1, 0, 0)), 0, List())
    val sceneToPrint = r.convertStateToScene(state)
    val printedScene = r.createScene(sceneToPrint)

    println(printedScene.mkString("\n"))
    println(twoPeopleExitingOnGroundFloor.mkString("\n"))

    printedScene shouldBe twoPeopleExitingOnGroundFloor
  }
}

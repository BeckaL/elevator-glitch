import org.scalatest.{FlatSpec, Matchers}

class IntegrationSpec extends FlatSpec with Matchers {

  "A simulator" should "convert an elevator state to a state to render" in {
    val state = ElevatorState(List(Person(0, 0, 0)), List(Lift(0, None, List(), "")), List(), 0, List())

    val r = SceneRenderer
    val sceneToPrint = r.convertStateToScene(state)
    sceneToPrint shouldBe SceneToRender(LiftToRender(0, 0, ""), List(PersonOnFloor(0)), List())
    val printedScene = r.createScene(sceneToPrint.copy(peopleWaiting = List(PersonOnFloor(1))))
    printedScene shouldBe partialSTate
  }

  val partialSTate = List(
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "─────────────────          ───── ",
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "                           ",
    "─────────────────          ───── ",
    "                           ",
    "                           ",
    "                  ╔══════╗ ",
    "                  ║      ║ ",
    "                  ║      ║ ",
    "                  ║      ║ ",
    "                  ║      ║ ",
    "              웃  ║      ║ ",
    "───────────────── ╚══════╝ ───── "

  )
}

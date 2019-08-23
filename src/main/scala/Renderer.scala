object Main extends App {
  def main(): Unit = {
    SceneRenderer.states.foreach(s => {
        println(s.mkString("\n")); Thread.sleep(500)
    })
  }
  main()
}

object SceneRenderer extends Scenery with Lifts with AsciiCharacters {
  def convertStateToScene(eState: ElevatorState): SceneToRender = {
    val firstLift = eState.lifts.head
    val lift = LiftToRender(firstLift.location.toInt, firstLift.people.size, firstLift.doorsOpen)
    val peopleWaiting = eState.peopleWaiting.map(person => PersonOnFloor(person.start))
    val exiters = eState.exiters.map(person => PersonOnFloor(person.start))
    SceneToRender(lift, peopleWaiting, exiters)
  }

  def createScene(scene: SceneToRender): List[String] = {
    val renderedLift: List[String] = scene.lift match {
      case _ if scene.lift.doorsOpen == "right" && scene.lift.people > 0 => liftWithRightDoorOpenAndPerson
      case _ if scene.lift.doorsOpen == "left" && scene.lift.people > 0 => liftWithLeftDoorOpenAndPerson
      case _ if scene.lift.people > 0 => liftWithPerson
      case _ if scene.lift.doorsOpen == "right" => liftWithRightDoorOpen
      case _ if scene.lift.doorsOpen == "left" => liftWithLeftDoorOpen
      case _ => emptyLift
    }

    val renderedWaitingSpace = (1 to 3).toList.map(x => if (peopleOnFloor(x, scene.peopleWaiting) > 0) waitingSpaceWithPerson else emptyWaitingSpace)
    val renderedExitSpaces = (1 to 3).toList.map(x => if (peopleOnFloor(x, scene.exiters) > 0) exitSpaceWithPerson else " ")

    def getWaitingSpaceString(n: Int): String = exitOrWaitingSpaceString(n, floorWaitingSpace, emptyWaitingSpace, renderedWaitingSpace)

    def exitSpaceString(n: Int): String = exitOrWaitingSpaceString(n, exitFloorSpace, " ", renderedExitSpaces)

    def getRowString(n: Int, lift: LiftToRender, renderedLift: List[String]): String =
      getWaitingSpaceString(n) + liftString(lift, renderedLift, n) + exitSpaceString(n)

    (0 to 25).map(getRowString(_, scene.lift, renderedLift)).toList.reverse
  }

  def exitOrWaitingSpaceString(n: Int, floor: String, emptySpace: String, renderedPersonSpace: List[String]) =
    n % 9 match {
      case 0 => floor
      case 1 => renderedPersonSpace(n / 9)
      case _ => emptySpace
    }

  private def liftString(lift: LiftToRender, renderedLift: List[String], position: Int): String =
    if ((0 to 6) contains (position - lift.position)) {
      renderedLift(6 - (position - lift.position))
    } else emptyLiftSpace

  private def peopleOnFloor(floor: Int, people: List[PersonOnFloor]): Int = people.count(p => p.floor == floor)

  val s1: List[String] = createScene(SceneToRender(LiftToRender(0, 0, ""), List(PersonOnFloor(1)), List()))
  val s2: List[String] = createScene(SceneToRender(LiftToRender(0, 0, "left"), List(PersonOnFloor(1)), List()))
  val s3: List[String] = createScene(SceneToRender(LiftToRender(0, 1, "left"), List(), List()))
  val s4: List[String] = createScene(SceneToRender(LiftToRender(0, 1, ""), List(), List()))
  val s5: List[String] = createScene(SceneToRender(LiftToRender(1, 1, ""), List(), List()))
  val s6: List[String] = createScene(SceneToRender(LiftToRender(2, 1, ""), List(), List()))
  val s7: List[String] = createScene(SceneToRender(LiftToRender(3, 1, ""), List(), List()))
  val s8: List[String] = createScene(SceneToRender(LiftToRender(4, 1, ""), List(), List()))
  val s9: List[String] = createScene(SceneToRender(LiftToRender(5, 1, ""), List(), List()))
  val s10: List[String] = createScene(SceneToRender(LiftToRender(6, 1, ""), List(), List()))
  val s11: List[String] = createScene(SceneToRender(LiftToRender(7, 1, ""), List(), List()))
  val s12: List[String] = createScene(SceneToRender(LiftToRender(8, 1, ""), List(), List()))
  val s13: List[String] = createScene(SceneToRender(LiftToRender(9, 1, ""), List(), List()))
  val s14: List[String] = createScene(SceneToRender(LiftToRender(9, 1, "right"), List(), List()))
  val s15: List[String] = createScene(SceneToRender(LiftToRender(9, 0, "right"), List(), List(PersonOnFloor(2))))
  val s16: List[String] = createScene(SceneToRender(LiftToRender(9, 0, ""), List(), List(PersonOnFloor(2))))
  val states = List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16)
}

case class LiftToRender(position: Int, people: Int, doorsOpen: String)

case class PersonOnFloor(floor: Int)

case class PeopleOnFloor(people: List[PersonOnFloor])

case class SceneToRender(lift: LiftToRender, peopleWaiting: List[PersonOnFloor], exiters: List[PersonOnFloor])


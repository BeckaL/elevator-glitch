package elevatorGlitch.Renderer

import elevatorGlitch.Simulator.{ElevatorState, LiftLocation}

object SceneRenderer extends AsciiCharacters with AsciiLifts {

  def convertStateToScene(eState: ElevatorState): SceneToRender = {
    val firstLift = eState.lifts.head
    val lift = LiftToRender(convertLiftLocationToInt(firstLift.location), firstLift.people.size, firstLift.doorsOpen)
    val peopleWaiting = eState.peopleWaiting.map(person => PersonOnFloor(person.start))
    val exiters = eState.exiters.map(person => PersonOnFloor(person.destination))
    SceneToRender(lift, peopleWaiting, exiters)
  }

  def convertLiftLocationToInt(location: LiftLocation): Int = location.floor * 9 + location.remainder

  def createScene(scene: SceneToRender): List[String] = {
    val renderedLift: List[String] = createLiftString(scene.lift.people, scene.lift.doorsOpen)
    val renderedWaitingSpace = (0 until 3).toList.map(x => waitingSpaceWithPerson(countPeopleOnFloor(x, scene.peopleWaiting)))
    val renderedExitSpaces = (0 until 3).toList.map(x => exitSpaceWithPerson(countPeopleOnFloor(x, scene.exiters)))

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

  private def countPeopleOnFloor(floor: Int, people: List[PersonOnFloor]): Int = people.count(p => p.floor == floor)

  case class LiftToRender(position: Int, people: Int, doorsOpen: String)

  case class PersonOnFloor(floor: Int)

  case class PeopleOnFloor(people: List[PersonOnFloor])

  case class SceneToRender(lift: LiftToRender, peopleWaiting: List[PersonOnFloor], exiters: List[PersonOnFloor])
}


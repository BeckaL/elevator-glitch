object Main extends App {
  def main(): Unit = {
    States.states.foreach(s => {
      println(s.mkString("\n")); Thread.sleep(500)
    })
  }
   main()
}

object States extends Scenery with Lifts {
  private def createScene(lift: LiftToRender, waiters: List[PersonOnFloor], exiters: List[PersonOnFloor]): List[String] = {
    val renderedLift: List[String] = lift match {
      case _ if lift.doorsOpen == "right" && lift.people > 0 => liftWithRightDoorOpenAndPerson
      case _ if lift.doorsOpen == "left" && lift.people > 0 => liftWithLeftDoorOpenAndPerson
      case _ if lift.people > 0 => liftWithPerson
      case _ if lift.doorsOpen == "right" => liftWithRightDoorOpen
      case _ if lift.doorsOpen == "left" => liftWithLeftDoorOpen
      case _ => emptyLift
    }

    val renderedWaitingSpace = (1 to 3).toList.map(x => if (peopleWaitingOnFloor(x, waiters) > 0) waitingSpaceWithPerson else emptyWaitingSpace)
    val renderedExitSpaces = (1 to 3).toList.map(x => if (peopleOnFloorWithCompleteJourneys(x, exiters) > 0) exitSpaceWithPerson else " ")

    def getRowString(n: Int, lift: LiftToRender, renderedLift: List[String]): String =
      getWaitingSpaceString(n) + liftString(lift, renderedLift, n) + exitSpaceString(n)

    def getWaitingSpaceString(n: Int): String = exitOrWaitingSpaceString(n, floorWaitingSpace, emptyWaitingSpace, renderedWaitingSpace)

    def exitSpaceString(n: Int): String = exitOrWaitingSpaceString(n, exitFloorSpace, " ", renderedExitSpaces)

    def exitOrWaitingSpaceString(n: Int, floor: String, emptySpace: String, renderedPersonSpace: List[String]) =
      n % 9 match {
        case 0 => floor
        case 1 => renderedPersonSpace(n / 9)
        case _ => emptySpace
      }

    (0 to 25).map(getRowString(_, lift, renderedLift)).toList.reverse
  }

  private def liftString(lift: LiftToRender, renderedLift: List[String], position: Int): String =
    if ((0 to 6) contains (position - lift.position)) {
      renderedLift(6 - (position - lift.position))
    } else emptyLiftSpace

  private def peopleWaitingOnFloor(floor: Int, peopleWaiting: List[PersonOnFloor]): Int =
    peopleWaiting.count(p => p.floor == floor)

  private def peopleOnFloorWithCompleteJourneys(floor: Int, peopleWithCompleteJourneys: List[PersonOnFloor]): Int =
    peopleWithCompleteJourneys.count(p => p.floor == floor)

  val s1: List[String] = createScene(LiftToRender(0, 0, ""), List(PersonOnFloor(1)), List())
  val s2: List[String] = createScene(LiftToRender(0, 0, "left"), List(PersonOnFloor(1)), List())
  val s3: List[String] = createScene(LiftToRender(0, 1, "left"), List(), List())
  val s4: List[String] = createScene(LiftToRender(0, 1, ""), List(), List())
  val s5: List[String] = createScene(LiftToRender(1, 1, ""), List(), List())
  val s6: List[String] = createScene(LiftToRender(2, 1, ""), List(), List())
  val s7: List[String] = createScene(LiftToRender(3, 1, ""), List(), List())
  val s8: List[String] = createScene(LiftToRender(4, 1, ""), List(), List())
  val s9: List[String] = createScene(LiftToRender(5, 1, ""), List(), List())
  val s10: List[String] = createScene(LiftToRender(6, 1, ""), List(), List())
  val s11: List[String] = createScene(LiftToRender(7, 1, ""), List(), List())
  val s12: List[String] = createScene(LiftToRender(8, 1, ""), List(), List())
  val s13: List[String] = createScene(LiftToRender(9, 1, ""), List(), List())
  val s14: List[String] = createScene(LiftToRender(9, 1, "right"), List(), List())
  val s15: List[String] = createScene(LiftToRender(9, 0, "right"), List(), List(PersonOnFloor(2)))
  val s16: List[String] = createScene(LiftToRender(9, 0, ""), List(), List(PersonOnFloor(2)))
  val states = List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16)

}

trait AsciiCharacters {
  val person = "\uC6C3"
  val bottomLeftJoin = "\u255a"
  val bottomRightJoin = "\u255d"
  val horizontalBorder = "\u2550"
  val verticalBorder = "\u2551"
  val topRightJoin = "\u2557"
  val topLeftJoin = "\u2554"
  val floor = "\u2500"

  def emptySpaceOfLength(n: Int): String = " " * n

  def floorSpaceOfLength(n: Int): String = floor * n
}

trait Lifts extends AsciiCharacters {
  private val topOfLift = topLeftJoin + horizontalBorder * 6 + topRightJoin
  private val middleOfLift = verticalBorder + " " * 6 + verticalBorder
  private val bottomOfLift = bottomLeftJoin + horizontalBorder * 6 + bottomRightJoin
  private val middleOfLiftWithLeftDoorOpen = " " * 7 + verticalBorder
  private val middleOfLiftWithRightDoorOpen = verticalBorder + " " * 7
  private val middleOfLiftWithPerson = verticalBorder + " " * 2 + person + " " * 2 + verticalBorder
  private val middleOfLIftWithRightDoorOpenAndPerson = verticalBorder + " " * 2 + person + " " * 3
  private val middleOfLiftWithLeftDoorOpenAndPerson = " " * 3 + person + " " * 2 + verticalBorder

  val emptyLift: List[String] = lift(middleOfLift, None)
  val liftWithLeftDoorOpen: List[String] = lift(middleOfLiftWithLeftDoorOpen, None)
  val liftWithRightDoorOpen: List[String] = lift(middleOfLiftWithRightDoorOpen, None)
  val liftWithPerson: List[String] = lift(middleOfLift, Some(middleOfLiftWithPerson))
  val liftWithRightDoorOpenAndPerson: List[String] = lift(middleOfLiftWithRightDoorOpen, Some(middleOfLIftWithRightDoorOpenAndPerson))
  val liftWithLeftDoorOpenAndPerson: List[String] = lift(middleOfLiftWithLeftDoorOpen, Some(middleOfLiftWithLeftDoorOpenAndPerson))

  private def lift(middle: String, bottom: Option[String]): List[String] =
    List(topOfLift, middle, middle, middle, middle, bottom.getOrElse(middle), bottomOfLift)
}

trait Scenery extends AsciiCharacters {
  val emptyWaitingSpace: String = emptySpaceOfLength(18)
  val floorWaitingSpace: String = floorSpaceOfLength(17) + emptySpaceOfLength(1)
  val waitingSpaceWithPerson: String = emptySpaceOfLength(14) + person + emptySpaceOfLength(2)
  val exitFloorSpace: String = emptySpaceOfLength(1) + floorSpaceOfLength(5) + emptySpaceOfLength(1)
  val exitSpaceWithPerson: String = emptySpaceOfLength(2) + person + emptySpaceOfLength(2)
  val emptyLiftSpace = emptySpaceOfLength(8)
}

case class LiftToRender(position: Int, people: Int, doorsOpen: String)

case class PersonOnFloor(floor: Int)

case class PeopleOnFloor(people: List[PersonOnFloor])
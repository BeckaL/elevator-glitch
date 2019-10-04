package elevatorGlitch.Renderer

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
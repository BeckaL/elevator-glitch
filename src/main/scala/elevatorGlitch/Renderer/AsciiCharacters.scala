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
  private val bottomOfLift = bottomLeftJoin + horizontalBorder * 6 + bottomRightJoin
  def createLiftString(n: Int, doorOpen: String) = {
    val leftPadding = if (n == 1) "  " else ""
    val rightPadding = if (n == 1) "  " else "  " * (3 - n)
    val leftBorder = if (doorOpen == "left") " " else verticalBorder
    val rightBorder = if (doorOpen == "right") " " else verticalBorder
    val bottomOfLift = leftBorder + leftPadding + person * n + rightPadding + rightBorder
    val middleOfLift = leftBorder + " " * 6 + rightBorder
    fullLiftString(middleOfLift, Some(bottomOfLift))
  }

  private def fullLiftString(middle: String, bottom: Option[String]): List[String] =
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
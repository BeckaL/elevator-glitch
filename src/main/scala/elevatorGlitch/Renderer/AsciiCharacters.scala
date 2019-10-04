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

  val emptyWaitingSpace: String = emptySpaceOfLength(18)
  val floorWaitingSpace: String = floorSpaceOfLength(17) + emptySpaceOfLength(1)
  val waitingSpaceWithPerson: String = emptySpaceOfLength(14) + person + emptySpaceOfLength(2)
  val exitFloorSpace: String = emptySpaceOfLength(1) + floorSpaceOfLength(5) + emptySpaceOfLength(1)
  val exitSpaceWithPerson: String = emptySpaceOfLength(2) + person + emptySpaceOfLength(2)
  val emptyLiftSpace = emptySpaceOfLength(8)
}
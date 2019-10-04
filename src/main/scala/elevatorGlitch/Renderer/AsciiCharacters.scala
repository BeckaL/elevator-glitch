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

  def waitingSpaceWithPerson(n: Int): String = emptySpaceOfLength(2 * (8 - n)) + person * n + emptySpaceOfLength(2)
  def exitSpaceWithPerson(n: Int): String =
    if (n==0) " " else emptySpaceOfLength(1) + person * n + emptySpaceOfLength(2 * (2 - n) + 1)

  val emptyWaitingSpace: String = emptySpaceOfLength(18)
  val floorWaitingSpace: String = floorSpaceOfLength(17) + emptySpaceOfLength(1)
  val exitFloorSpace: String = emptySpaceOfLength(1) + floorSpaceOfLength(5) + emptySpaceOfLength(1)
  val emptyLiftSpace = emptySpaceOfLength(8)
}

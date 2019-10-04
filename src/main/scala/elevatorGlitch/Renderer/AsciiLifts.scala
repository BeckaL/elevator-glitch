package elevatorGlitch.Renderer

trait AsciiLifts extends AsciiCharacters {
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
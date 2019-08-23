class Runner(randomiser: Randomiser, printer: Printer) {
  val renderer = SceneRenderer
  val simulator = new Simulator(1, 3, randomiser)

  def run(time: Int = 0, maxTicks: Int, state: ElevatorState = simulator.initialTick): ElevatorState = {
    val newState = simulator.nextTick(state, time)
    printState(newState)
    val newTime = time + 1
    if (newTime < maxTicks) run(newTime, maxTicks, newState) else {
      newState
    }
  }

  def printState(elevatorState: ElevatorState): Unit = {
    val stateToPrint = renderer.convertStateToScene(elevatorState)
    printer.print(renderer.createScene(stateToPrint))
  }
}

trait Printer {
  def print(string: List[String]): Unit
}

object StdOutPrinter extends Printer {
  def print(strings: List[String]) = println(strings.mkString("\n"))
}
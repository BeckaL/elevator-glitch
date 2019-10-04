import elevatorGlitch.Renderer.SceneRenderer
import elevatorGlitch.Simulator.{ElevatorState, Randomiser, Simulator}

class Runner(randomiser: Randomiser, printer: Printer) {
  val renderer = SceneRenderer
  val simulator = new Simulator(1, 3, randomiser)

  def run(time: Int = 0, maxTicks: Int, state: ElevatorState = simulator.initialTick, sleep: Int = 0): ElevatorState = {
    val newState = simulator.nextTick(state, time)
    printState(newState)
    Thread.sleep(sleep)
    val newTime = time + 1
    if (newTime < maxTicks) run(newTime, maxTicks, newState, sleep) else {
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
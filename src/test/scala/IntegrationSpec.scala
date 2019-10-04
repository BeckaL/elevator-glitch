import elevatorGlitch.Simulator.{ElevatorState, Lift, LiftLocation, Randomiser}
import org.scalatest.{FlatSpec, Matchers}

class IntegrationSpec extends FlatSpec with Matchers with CompletePrintedJourney {

    "A runner" should "complete a full run" in {
      val myPrinter = new mockPrinter
      val r = new Runner(new mockRandomiserGeneratingOnePerson, myPrinter)
      val initialState = ElevatorState(peopleWaiting = List(), lifts = List(Lift(LiftLocation(0, 0), None, List(), "")), exiters = List(), time = 0, journeyHistory = List())

      r.run(0, 15, initialState)
      myPrinter.output should be(states.take(15).map(_.mkString("\n")))
    }

  class mockPrinter extends Printer {
    var output: List[String] = List()

    override def print(string: List[String]): Unit =
      output :+= string.mkString("\n")
  }

  object mockRandomiser extends Randomiser {
    override def randomDestination(location: Int, floors: Int): Int = 2

    override def randomNumberOfPeople(): Int = 1
  }

  object mockRandomiserGeneratingNoPeople extends Randomiser {
    override def randomDestination(location: Int, floors: Int): Int = 0

    override def randomNumberOfPeople(): Int = 0
  }

  class mockRandomiserGeneratingOnePerson extends Randomiser {
    var timesCalled = 0

    override def randomDestination(location: Int, floors: Int): Int = 1

    override def randomNumberOfPeople(): Int = if (timesCalled == 0) {
      timesCalled += 1;
      1
    } else 0
  }

}

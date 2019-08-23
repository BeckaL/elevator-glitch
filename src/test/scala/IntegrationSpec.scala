import org.scalatest.{FlatSpec, Matchers}

class IntegrationSpec extends FlatSpec with Matchers with CompletePrintedJourney {


  "A runner" should "complete a full run" in {
    val myPrinter = new mockPrinter
    val randomiser = new mockRandomiserGeneratingOnePerson
    val r = new Runner(randomiser, myPrinter)
    val lifts = List(Lift(0.0, None, List(), ""))
    val initialState = ElevatorState(peopleWaiting = List(), lifts = List(Lift(0.0, None, List(), "")), exiters = List(), time = 0, journeyHistory = List())

    r.run(0, 3, initialState)
    myPrinter.output should be(List(stateOne.mkString("\n"), stateTwo.mkString("\n"), stateThree.mkString("\n")))
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

    override def randomDestination(location: Int, floors: Int): Int = 2

    override def randomNumberOfPeople(): Int = if (timesCalled == 0) {
      timesCalled += 1;
      1
    } else 0
  }

}

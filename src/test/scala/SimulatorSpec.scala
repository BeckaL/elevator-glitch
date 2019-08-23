import org.scalatest._

class SimulatorSpec extends FlatSpec with Matchers {

  "A simulator" should "be generated with a number of floors and lifts" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)

    s.floors shouldBe 5
    s.lifts shouldBe 2
  }

  "A simulator" should "start with lifts on the ground floor with no destination or people, and no people waiting" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)
    val state = s.initialTick

    state.lifts shouldBe List(Lift(0, None, List(), ""), Lift(0, None, List(), ""))
    state.peopleWaiting shouldBe List()
  }

  it should "generate new people with a destination and a location at a new tick" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)
    val people = mockRandomiser.generatePeople(0, 5)

    assert(people.size == 6)
    assert(people(0).start == 0)
    assert(people(0).destination == 2)
  }

  it should "people wait for one tick after generation before getting in a lift" in {
    val s = new Simulator(floors = 5, lifts = 1, randomiser = mockRandomiserGeneratingOnePerson)
    val initialState = s.initialTick
    val stateOne = s.nextTick(initialState, 1)
    val stateTwo = s.nextTick(stateOne, 2)

    stateOne.peopleWaiting shouldBe List(Person(0, 2, 1))
    stateTwo.lifts shouldBe List(Lift(0, Some(2), List(Person(0, 2, 1)), ""))
    stateTwo.peopleWaiting shouldBe List()
  }

  it should "move a lift should move towards its destination floor" in {
    val s = new Simulator(floors = 5, lifts = 1, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(noPeopleWaiting, lifts = List(Lift(0, Some(2), List(Person(0, 2, 1)), "")), noExiters, time = 1, emptyJourneyHistory)
    val stateOne = s.nextTick(initialState, 2)

    stateOne.lifts shouldBe List(Lift(0.5, Some(2), List(Person(0, 2, 1)), ""))
  }

  it should "stop a lift at its destination floor and empty of people" in {
    val s = new Simulator(floors = 5, lifts = 1, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(peopleWaiting = List(), lifts = List(Lift(1, Some(2), List(Person(0, 2, 1)), "")), time = 1)
    val finalState = s.run(0, 2, initialState)

    finalState.lifts shouldBe List(Lift(2, None, List(), ""))
  }

  it should "load two lifts at different locations" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(peopleWaiting = List(Person(0, 2, 1), Person(5, 1, 1)), lifts = List(Lift(0, None, List(), ""), Lift(5, None, List(), "")), noExiters, time = 1, emptyJourneyHistory)
    val stateOne = s.nextTick(initialState, 2)

    stateOne.lifts shouldBe List(Lift(0, Some(2), List(Person(0, 2, 1)), ""), Lift(5, Some(1), List(Person(5, 1, 1)), ""))
  }

  it should "save a person's journey once they disembark" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(peopleWaiting = List(), lifts = List(Lift(5, None, List(Person(1, 5, 0)), "")), time = 1)
    val stateOne = s.nextTick(initialState, 2)

    stateOne.journeyHistory shouldBe List(JourneyHistory(startFloor = 1, endFloor = 5, startTime = 0, endTime = 2))
  }

  it should "a lift opens its door to let people board" in {
    val s = new Simulator(floors = 3, lifts = 1, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(peopleWaiting = List(Person(1, 2, 0)), lifts = List(Lift(0.0, None, List(), "")),noExiters, 0, emptyJourneyHistory)

    val nextState = s.nextTick(initialState, time = 1)
    nextState.lifts shouldBe List(Lift(0.0, None, List(), "left"))
  }

  object mockRandomiser extends Randomiser {
    override def randomDestination(location: Int, floors: Int): Int = 2

    override def randomNumberOfPeople(): Int = 1
  }

  object mockRandomiserGeneratingNoPeople extends Randomiser {
    override def randomDestination(location: Int, floors: Int): Int = 0

    override def randomNumberOfPeople(): Int = 0
  }

  object mockRandomiserGeneratingOnePerson extends Randomiser {
    var timesCalled = 0

    override def randomDestination(location: Int, floors: Int): Int = 2

    override def randomNumberOfPeople(): Int = if (timesCalled == 0) {
      timesCalled += 1; 1
    } else 0
  }

  private val noPeopleWaiting = List()
  private val noExiters = List()
  private val emptyJourneyHistory = List()

}
import elevatorGlitch.Simulator.{ElevatorState, JourneyHistory, Lift, LiftLocation, Person, Randomiser, Simulator}
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

    state.lifts shouldBe List(Lift(LiftLocation(0, 0), None, List(), ""), Lift(LiftLocation(0, 0), None, List(), ""))
    state.peopleWaiting shouldBe List()
  }

  it should "generate new people with a destination and a location at a new tick" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)
    val people = mockRandomiser.generatePeople(0, 5)

    assert(people.size == 5)
    assert(people(0).start == 0)
    assert(people(0).destination == 2)
  }

  it should "an elevator with no destination opens its doors if there's someone waiting where it is" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(
      List(Person(0, 1, 0)),
      List(emptyLiftOnGroundFloorWithNoDestination),
      List(),
      0,
      List())
    val stateOne = s.nextTick(initialState, 1)

    stateOne.peopleWaiting shouldBe List(Person(0, 1, 0))
    stateOne.lifts shouldBe List(emptyLiftOnGroundFloorWithLeftDoorOpen)
  }

  it should "a person should enter an elevator with its left door open and set the destination" in {
    val s = new Simulator(floors = 5, lifts = 1, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(List(waiterOnGroundFloorGoingToFirstFloor), lifts = List(emptyLiftOnGroundFloorWithLeftDoorOpen), noExiters, time = 0, emptyJourneyHistory)
    val stateOne = s.nextTick(initialState, 1)

    stateOne.lifts shouldBe List(liftWithPersonGoingToFirstFloorLeftDoorOpen)
  }

  it should "close its left door once people have boarded" in {
    val s = new Simulator(floors = 5, lifts = 1, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(peopleWaiting = List(), lifts = List(liftWithPersonGoingToFirstFloorLeftDoorOpen), time = 0)
    val stateOne = s.nextTick(initialState, 1)


    stateOne.lifts shouldBe List(liftWithPersonGoingToFirstFloor)
  }

  it should "move towards its destination with people inside" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(peopleWaiting = List(), lifts = List(liftWithPersonGoingToFirstFloor), noExiters, time = 0, emptyJourneyHistory)
    val stateOne = s.nextTick(initialState, 1)

    stateOne.lifts shouldBe List(movingLiftWithPerson)
  }

  it should "stop at the destination floor and open its doors" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(peopleWaiting = List(), lifts = List(movingLiftAtDestination), noExiters, time = 0, emptyJourneyHistory)
    val stateOne = s.nextTick(initialState, 1)

    stateOne.lifts shouldBe List(liftAtDestinationWithRightDoorOpen)
  }

  it should "unload a person with a complete journey" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiserGeneratingNoPeople)
    val initialState = ElevatorState(peopleWaiting = List(), lifts = List(liftAtDestinationWithRightDoorOpen), noExiters, time = 0, emptyJourneyHistory)
    val stateOne = s.nextTick(initialState, 1)

    stateOne.lifts shouldBe List(emptyLiftOnFirstFloorWithRightDoorOpen)
    stateOne.exiters shouldBe List(Person(0, 1, 0))
    stateOne.journeyHistory shouldBe List(JourneyHistory(startFloor = 0, endFloor = 1, startTime = 0, endTime = 1))
  }

  private val emptyLiftOnGroundFloorWithNoDestination = Lift(LiftLocation(0, 0), None, List(), "")
  private val emptyLiftOnGroundFloorWithLeftDoorOpen = emptyLiftOnGroundFloorWithNoDestination.copy(doorsOpen = "left")
  private val waiterOnGroundFloorGoingToFirstFloor = Person(0, 1, 0)
  private val liftWithPersonGoingToFirstFloorLeftDoorOpen = emptyLiftOnGroundFloorWithLeftDoorOpen.copy(people = List(waiterOnGroundFloorGoingToFirstFloor), destination = Some(waiterOnGroundFloorGoingToFirstFloor.destination))
  private val liftWithPersonGoingToFirstFloor = liftWithPersonGoingToFirstFloorLeftDoorOpen.copy(doorsOpen = "")
  private val movingLiftWithPerson = liftWithPersonGoingToFirstFloor.copy(location = LiftLocation(0, 1))
  private val movingLiftAtDestination = liftWithPersonGoingToFirstFloor.copy(location = LiftLocation(1, 0))
  private val liftAtDestinationWithRightDoorOpen = movingLiftAtDestination.copy(doorsOpen = "right")
  private val emptyLiftOnFirstFloorWithRightDoorOpen = liftAtDestinationWithRightDoorOpen.copy(people = List())
  private val noPeopleWaiting = List()
  private val noExiters = List()
  private val emptyJourneyHistory = List()

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
      timesCalled += 1;
      1
    } else 0
  }

}
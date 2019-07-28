class Simulator(val lifts: Int,
                val floors: Int,
                randomiser: Randomiser,
                criteria: ScenarioCriteria = BasicCriteria) extends ElevatorObject {

  def run(time: Int = 0, maxTicks: Int, state: ElevatorState = initialTick()): ElevatorState = {
    val newState = nextTick(state, time)
    val newTime = time + 1
    if (newTime < maxTicks) run(newTime, maxTicks, newState) else newState
  }

  def initialTick(): ElevatorState =
    ElevatorState(lifts = List.fill(lifts)(Lift(0, None, List())), peopleWaiting = List(), time = 0)

  def nextTick(state: ElevatorState, time: Int): ElevatorState = {
    val peopleInLiftsAtTickStart = peopleInLifts(state.lifts)
    val loadedLifts = criteria.loadPeople(updateLifts(state.lifts), state.peopleWaiting)
    val peopleWithCompletedJourneys = peopleInLiftsAtTickStart.filterNot(peopleInLifts(loadedLifts).contains(_))
    val peopleStillWaitingAfterLoading = state.peopleWaiting.filterNot(peopleInLifts(loadedLifts).contains(_))
    val newPeopleWaiting = peopleStillWaitingAfterLoading ++ randomiser.generatePeople(time, this.floors)
    ElevatorState(lifts = loadedLifts,
      peopleWaiting = newPeopleWaiting,
      time = time,
      journeyHistory = state.journeyHistory ++ peopleWithCompletedJourneys.map(registerJourney(_, time)))
  }

  def peopleInLifts(lifts: Lifts): People = lifts.flatMap(_.people)

  def updateLifts(lifts: Lifts): Lifts = lifts.map(lift => lift.moveOne().updateDestination().empty())

  def registerJourney(person: Person, time: Int): Map[String, Int] =
    Map("startFloor" -> person.start, "endFloor" -> person.destination, "startTime" -> person.startTime, "endTime" -> time)
}

trait ElevatorObject {
  type Lifts = List[Lift]
  type People = List[Person]
}

case class ElevatorState(peopleWaiting: List[Person], lifts: List[Lift], time: Int, journeyHistory: List[Map[String, Int]] = List())

case class Person(start: Int, destination: Int, startTime: Int) extends ElevatorObject

case class Lift(location: Double, destination: Option[Int], people: List[Person]) extends ElevatorObject {
  def moveOne(): Lift = if (destination.isDefined) this.copy(location = oneTowardsDestination()) else this

  def updateDestination(): Lift = if (atDestination()) this.copy(destination = None) else this

  def empty(): Lift = this.copy(people = people.filter(_.destination != location))

  private def atDestination(): Boolean = destination.isDefined && location == destination.get.toDouble

  private def oneTowardsDestination(): Double = if (destination.get < location) location - 0.5 else location + 0.5
}


trait ScenarioCriteria extends ElevatorObject {
  def loadPeople(lifts: Lifts, peopleWaiting: People): Lifts
}

trait Randomiser extends ElevatorObject {
  def randomDestination(location: Int, floors: Int): Int

  def randomNumberOfPeople(): Int

  def generatePeople(time: Int, floors: Int): People = {
    (0 to floors).toList.flatMap(floor => {
      val noOfPeople = randomNumberOfPeople()
      List.fill(noOfPeople)(
        Person(start = floor,
          destination = randomDestination(floor, floors),
          startTime = time))
    })
  }
}
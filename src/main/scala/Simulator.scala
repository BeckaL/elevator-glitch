class Simulator(val lifts: Int,
                val floors: Int,
                randomiser: Randomiser,
                criteria: ScenarioCriteria = BasicCriteria) extends ElevatorObject {

  def run(time: Int = 0, maxTicks: Int, state: ElevatorState = initialTick): ElevatorState = {
    val newState = nextTick(state, time)
    val newTime = time + 1
    if (newTime < maxTicks) run(newTime, maxTicks, newState) else newState
  }

  val initialTick =
    ElevatorState(lifts = List.fill(lifts)(Lift(0, None, List(), "")), peopleWaiting = List(), time = 0)

//  def nextTick(state: ElevatorState, time: Int): ElevatorState = {
//    val peopleInLiftsAtTickStart = peopleInLifts(state.lifts)
//    val liftUpdatedWithNextAction: Lifts = updateLiftState(state.lifts, state.peopleWaiting)
//    val loadedLifts = criteria.loadPeople(updateLifts(liftUpdatedWithNextAction, state.peopleWaiting), state.peopleWaiting)
//    val peopleWithCompletedJourneys: People = peopleInLiftsAtTickStart.filterNot(peopleInLifts(loadedLifts).contains(_))
//    val peopleStillWaitingAfterLoading = state.peopleWaiting.filterNot(peopleInLifts(loadedLifts).contains(_))
//    val newPeopleWaiting = peopleStillWaitingAfterLoading ++ randomiser.generatePeople(time, this.floors)
//    ElevatorState(lifts = loadedLifts,
//      peopleWaiting = newPeopleWaiting,
//      time = time,
//      journeyHistory = state.journeyHistory ++ peopleWithCompletedJourneys.map(p => registerJourney(p, time)))
//  }

//  def peopleInLifts(lifts: Lifts): People = lifts.flatMap(_.people)

  def nextTick(state: ElevatorState, time: Int): ElevatorState = {
    val liftsUpdatedWithNextAction: Lifts = updateLiftState(state.lifts, state.peopleWaiting)
    val updatedLifts = updateLiftState(state.lifts, state.peopleWaiting)
    val newLifts = updateLifts(updatedLifts, state.peopleWaiting)
    val newPeopleWaiting = randomiser.generatePeople(time, this.floors)
        ElevatorState(lifts = newLifts,
          peopleWaiting = state.peopleWaiting ++ newPeopleWaiting,
          time = time,
          journeyHistory = state.journeyHistory)
      }


  def updateLiftState(lifts: List[Lift], peopleWaiting: List[Person]): List[Lift] = lifts.map(lift => lift.copy(state = lift.updateNextAction(peopleWaiting)))

  def updateLifts(lifts: List[Lift], peopleWaiting: List[Person]): List[Lift] =
    lifts.map(lift => if (lift.state == "Opening Left Door") lift.openDoors() else {
        lift.updateDestination().moveOne().empty()
      })


  def registerJourney(p: Person, time: Int): JourneyHistory =
    JourneyHistory(startFloor = p.start, endFloor = p.destination, startTime = p.startTime, endTime = time)
}

trait ElevatorObject {
  type Lifts = List[Lift]
  type People = List[Person]
}

case class ElevatorState(peopleWaiting: List[Person], lifts: List[Lift], exiters: List[Person] = List(), time: Int, journeyHistory: List[JourneyHistory] = List())

case class JourneyHistory(startFloor: Int, endFloor: Int, startTime: Int, endTime: Int)

case class Person(start: Int, destination: Int, startTime: Int) extends ElevatorObject

case class Lift(location: Double, destination: Option[Int], people: List[Person], doorsOpen: String, state: String = "") extends ElevatorObject {
  def updateNextAction(peopleWaiting: People): String = this match {
    case _ if destination.isEmpty && peopleWaiting.exists(p=> p.start.toDouble - 1 == location) => "Opening Left Door"
    case _ => ""
  }

  def moveOne(): Lift = if (destination.isDefined) this.copy(location = oneTowardsDestination()) else this

  def openDoors(): Lift = this.copy(doorsOpen = "left")

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
    (1 to floors).toList.flatMap(floor => {
      val noOfPeople = randomNumberOfPeople()
      List.fill(noOfPeople)(
        Person(start = floor,
          destination = randomDestination(floor, floors),
          startTime = time))
    })
  }
}

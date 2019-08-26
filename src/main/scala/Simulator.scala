class Simulator(val lifts: Int,
                val floors: Int,
                randomiser: Randomiser,
                criteria: ScenarioCriteria = BasicCriteria) extends ElevatorObject {

  val initialTick =
    ElevatorState(lifts = List.fill(lifts)(Lift(LiftLocation(0, 0), None, List(), "")), peopleWaiting = List(), time = 0)

  def peopleInLifts(lifts: Lifts): People = lifts.flatMap(_.people)

  def nextTick(state: ElevatorState, time: Int): ElevatorState = {
    val peopleInLiftsAtTickStart = peopleInLifts(state.lifts)
    val newLifts = updateLifts(state.lifts, state.peopleWaiting)
    val peopleWithCompletedJourneys: People = peopleInLiftsAtTickStart.filterNot(peopleInLifts(newLifts).contains(_))
    val peopleStillWaitingAfterLoading = state.peopleWaiting.filterNot(peopleInLifts(newLifts).contains(_))
    val newPeopleWaiting = randomiser.generatePeople(time, this.floors)
        ElevatorState(lifts = newLifts,
          peopleWaiting = peopleStillWaitingAfterLoading ++ newPeopleWaiting,
          time = time,
          journeyHistory = state.journeyHistory ++ peopleWithCompletedJourneys.map(p => registerJourney(p, time)),
          exiters = peopleWithCompletedJourneys)
      }

  def updateLifts(lifts: List[Lift], peopleWaiting: List[Person]): List[Lift] =
    lifts.map(lift => lift.updateNextAction(peopleWaiting, criteria))

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

case class Lift(location: LiftLocation, destination: Option[Int], people: List[Person], doorsOpen: String) extends ElevatorObject {
  def updateNextAction(peopleWaiting: People, criteria: ScenarioCriteria): Lift = this match {
    case _ if destination.isEmpty && peopleWaiting.exists(p=> p.start  == location.floor && location.remainder == 0) && doorsOpen == "left" => criteria.loadPeople(this, peopleWaiting)
    case _ if destination.isEmpty && peopleWaiting.exists(p=> p.start  == location.floor && location.remainder == 0) => this.openDoors("left")
    case _ if destination.isDefined && doorsOpen == "left" => this.closeDoor()
    case _ if atDestination() && doorsOpen == "right" => this.unload
    case _ if atDestination() => this.openDoors("right")
    case _ if destination.isDefined && doorsOpen == "" => this.moveOne()
    case _ if destination.isDefined && location.remainder > 0 => this.moveOne()
    case _ => this
  }

  def openDoors(side: String): Lift = this.copy(doorsOpen = side)

  def closeDoor(): Lift = this.copy(doorsOpen = "")

  def updateDestination(): Lift = if (atDestination()) this.copy(destination = None) else this

  def unload():Lift = this.copy(people = people.filterNot(p => p.destination == this.location.floor))

  def atDestination(): Boolean = destination.isDefined && location.floor == destination.get && location.remainder == 0

  def moveOne(): Lift = {
    val oneUp = LiftLocation.convertIntToLiftLocation(location.convertToInt + 1)
    val oneDown = LiftLocation.convertIntToLiftLocation(location.convertToInt - 1)
    this.copy(location = if (destination.get > location.floor) oneUp else oneDown)
  }
}

trait ScenarioCriteria extends ElevatorObject {
  def loadPeople(lift: Lift, peopleWaiting: People): Lift
}

trait Randomiser extends ElevatorObject {
  def randomDestination(location: Int, floors: Int): Int

  def randomNumberOfPeople(): Int

  def generatePeople(time: Int, floors: Int): People = {
    (0 until floors).toList.flatMap(floor => {
      val noOfPeople = randomNumberOfPeople()
      List.fill(noOfPeople)(
        Person(start = floor,
          destination = randomDestination(floor, floors),
          startTime = time))
    })
  }
}

case class LiftLocation(floor: Int, remainder: Int) {
  def convertToInt = floor * 9 + remainder
}

object LiftLocation {
  def convertIntToLiftLocation(floorInInt: Int): LiftLocation =
    LiftLocation(floorInInt / 9, floorInInt % 9)
}

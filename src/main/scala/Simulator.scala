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
    val loadedLifts = criteria.loadCriteria(updateLifts(state.lifts), state.peopleWaiting)
    val newPeopleWaiting = state.peopleWaiting.filterNot(peopleInLifts(loadedLifts).contains(_)) ++ generatePeople(time)
    ElevatorState(lifts = loadedLifts, peopleWaiting = newPeopleWaiting, time = time)
  }

  def peopleInLifts(lifts: Lifts): People = lifts.flatMap(l => l.people)

  def updateLifts(lifts: Lifts): Lifts = lifts.map(lift => lift.moveOne().updateDestination().empty())

  def generatePeople(time: Int): People = {
    val people = for (floor <- 0 to this.floors) yield {
      val noOfPeople = randomiser.randomNumberOfPeople()
      List.fill(noOfPeople)(Person(start = floor, destination = randomiser.randomDestination(floor, this.floors), startTime = time))
    }
    people.toList.flatten
  }
}

trait Randomiser{
  def randomDestination(location: Int, floors: Int): Int
  def randomNumberOfPeople(): Int
}

trait ElevatorObject {
  type Lifts = List[Lift]
  type People = List[Person]
}

case class ElevatorState(peopleWaiting: List[Person], lifts: List[Lift], time: Int)

case class Lift(location: Double, destination: Option[Int], people: List[Person]) extends ElevatorObject {
  def moveOne(): Lift = {
    if (destination.isDefined) {
      this.copy(location = if (destination.get < location) location - 0.5 else location + 0.5)
    } else this
  }

  def updateDestination(): Lift = {
    if (destination.isDefined  && location == destination.get.toDouble) {
      this.copy(destination = None)
    } else this
  }

  def empty(): Lift = {
    val peopleSeparated = people.partition(p => p.destination == location)
    val disembarkingPeople = peopleSeparated._1
    val remainingPeople = peopleSeparated._2
    this.copy(people = remainingPeople)
  }
}

case class Person(start: Int, destination: Int, startTime: Int) extends ElevatorObject

object scalaRandomizer extends Randomiser {
  val r = scala.util.Random
  def randomDestination(location: Int, floors: Int): Int = {
    val viableFloors = (0 to floors).toSet diff Set(location)
    viableFloors.toVector(r.nextInt(viableFloors.size))
  }
  def randomNumberOfPeople(): Int =
    r.nextInt(100) match {
      case x if x < 35 => 0
      case x if x < 55 => 1
      case x if x < 75 => 2
      case x if x < 90 => 3
      case x if x < 97 => 4
      case _ => 5
    }
}

trait ScenarioCriteria extends ElevatorObject {
  def loadCriteria(lifts: Lifts, peopleWaiting: People): Lifts
}

object BasicCriteria extends ScenarioCriteria {

  override def loadCriteria(lifts: Lifts, peopleWaiting: People): List[Lift] = lifts.map(l => loadLift(l, peopleWaiting))

  def loadLift(lift: Lift, peopleWaiting: People): Lift = {
    val peopleToLoad = peopleWaiting.filter(p => p.start == lift.location)
    if (peopleToLoad.nonEmpty) {
      val nearestDestinationOfPeople = peopleToLoad.map(_.destination).minBy(d => difference(lift.location, d))
      val nextDestination = if (lift.destination.nonEmpty) {
        if (difference(lift.location, lift.destination.get) < difference(lift.location, nearestDestinationOfPeople)) {
          lift.destination
        } else Some(nearestDestinationOfPeople)
      } else Some(nearestDestinationOfPeople)
      lift.copy(people = lift.people ++ peopleToLoad, destination = nextDestination)
    } else lift
  }

  private def difference(location: Double, destination: Int): Double = (location - destination).abs
}


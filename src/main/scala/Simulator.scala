class Simulator(val lifts: Int, val floors: Int, randomiser: Randomiser = scalaRandomizer) {
  def initialTick(): ElevatorState =
    ElevatorState(lifts = List.fill(lifts)(Lift(0, None, List())), peopleWaiting = List(), time = 0)

  def nextTick(previousState: ElevatorState, time: Int): ElevatorState = {
    val movedLifts = previousState.lifts.map(lift => moveLifts(lift))
    val emptiedLifts = movedLifts.map(emptyLift)
    val peopleWaiting = previousState.peopleWaiting
    val generatedWaiters = generatePeople(peopleWaiting, time)
    if (!peopleWaiting.isEmpty) {
      val person: Person = peopleWaiting.head
      val lift: Lift = emptiedLifts.head
      if (lift.location == person.start) {
        val newLift = lift.copy(destination = Some(person.destination), people = List(person))
        val newPeopleWaiting = generatedWaiters.filterNot(p => p == person)
        val newState = ElevatorState(lifts = List(newLift),peopleWaiting = newPeopleWaiting, time = time)
        newState
      } else previousState.copy(peopleWaiting = generatedWaiters, lifts = emptiedLifts)
    } else previousState.copy(peopleWaiting = generatedWaiters, lifts = emptiedLifts)
  }

  def moveLifts(lift: Lift): Lift =
    if (lift.destination.isDefined) {
      val liftDirection = if (lift.destination.get < lift.location) "down" else "up"
      val liftWithNewLocation = if (liftDirection == "down") lift.copy(lift.location - 0.5) else lift.copy(lift.location + 0.5)
      if (liftWithNewLocation.location == liftWithNewLocation.destination.get.toDouble) liftWithNewLocation.copy(destination = None) else liftWithNewLocation
    } else lift

  def emptyLift(lift: Lift): Lift = {
    val people = lift.people.partition(p => p.destination == lift.location)
    val disembarkingPeople = people._1
    val remainingPeople = people._2
    lift.copy(people = remainingPeople)
  }

  def generatePeople(existingPeople: List[Person], time: Int): List[Person] = {
    val floors = this.floors
    val people = for (floor <- 0 to floors) yield {
      val noOfPeople = randomiser.randomNumberOfPeople()
      List.fill(noOfPeople)(Person(start = floor, destination = randomiser.randomDestination(floor, this.floors), startTime = time))
    }
    existingPeople ++ people.toList.flatten
  }
}

trait Randomiser{
  def randomDestination(location: Int, floors: Int): Int
  def randomNumberOfPeople(): Int
}


trait ElevatorObject

case class ElevatorState(peopleWaiting: List[Person], lifts: List[Lift], time: Int)

case class Lift(location: Double, destination: Option[Int], people: List[Person]) extends ElevatorObject

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


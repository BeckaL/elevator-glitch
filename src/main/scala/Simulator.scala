class Simulator(val lifts: Int, val floors: Int, randomiser: Randomiser = scalaRandomizer) {
  def initialTick(): ElevatorState =
    ElevatorState(lifts = List.fill(lifts)(Lift(0, None, List())), peopleWaiting = List(), time = 0)

  def nextTick(previousState: ElevatorState, time: Int): ElevatorState = {
    println(s"time is $time")

    val peopleWaiting = previousState.peopleWaiting
    println(s"peopleWaiting are $peopleWaiting")

    val generatedWaiters = generatePeople(peopleWaiting, time)
    println(s"generatedWaiters are $generatedWaiters")

    if (!peopleWaiting.isEmpty) {
      val person: Person = peopleWaiting.head
      val lift: Lift = previousState.lifts.head
      if (lift.location == person.start) {
        println("person gets in lift")
        val newLift = lift.copy(destination = Some(person.destination), people = List(person))
        println(s"newLift is $newLift")
        val newPeopleWaiting = generatedWaiters.filterNot(p => p == person)
        println(s"newPeopleWaiting are $newPeopleWaiting")
        val newState = ElevatorState(lifts = List(newLift),peopleWaiting = newPeopleWaiting, time = time)
        println(s"newState is $newState")
        newState
      } else previousState.copy(peopleWaiting = generatedWaiters)
    } else previousState.copy(peopleWaiting = generatedWaiters)
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

case class Lift(location: Int, destination: Option[Int], people: List[Person]) extends ElevatorObject

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


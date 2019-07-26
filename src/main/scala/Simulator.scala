class Simulator(val lifts: Int, val floors: Int, randomiser: Randomiser = scalaRandomizer) {
  def initialTick(): Map[String, List[ElevatorObject]] =
    Map("lifts" -> List.fill(lifts)(Lift(0, None, List())), "people" -> List())

  def generatePeople(existingPeople: List[Person]): List[Person] = {
    val floors = this.floors
    val people = for (floor <- 0 to floors) yield {
      val noOfPeople = randomiser.randomNumberOfPeople()
      List.fill(noOfPeople)(Person(start = floor, destination = randomiser.randomDestination(floor, this.floors)))
    }
    people.toList.flatten
  }
}

trait Randomiser{
  def randomDestination(location: Int, floors: Int): Int
  def randomNumberOfPeople(): Int
}

trait ElevatorObject

case class Lift(location: Int, destination: Option[Int], people: List[Person]) extends ElevatorObject

case class Person(start: Int, destination: Int) extends ElevatorObject

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


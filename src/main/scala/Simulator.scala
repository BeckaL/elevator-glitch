class Simulator(val lifts: Int, val floors: Int, randomiser: Randomiser) {
  private def randomDestination(location: Int): Int = {
    val viableFloors = (0 to this.floors).toSet diff Set(location)
    viableFloors.toVector(randomiser.nextInt(viableFloors.size))
  }

  def generatePeople(existingPeople: List[Person]): List[Person] = {
    val floors = this.floors
    val people = for (floor <- 0 to floors) yield {
      val noOfPeople = numberToPersonMappings(randomiser.nextInt(100))
      List.fill(noOfPeople)(Person(start = floor, destination = randomDestination(floor)))
    }
    people.toList.flatten
  }

    private def numberToPersonMappings(n: Int): Int = {
      n match {
        case x if x < 35 => 0
        case x if x < 55 => 1
        case x if x < 75 => 2
        case x if x < 90 => 3
        case x if x < 97 => 4
        case _ => 5
      }
    }
}

trait Randomiser{
  def nextInt(i: Int): Int
}


case class Person(start: Int, destination: Int)

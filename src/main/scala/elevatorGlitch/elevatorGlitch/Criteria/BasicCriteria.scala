package elevatorGlitch.elevatorGlitch.Criteria

import elevatorGlitch.Simulator.{Lift, Person}

object BasicCriteria extends ScenarioCriteria {

  override def loadPeople(lift: Lift, peopleWaiting: List[Person]): Lift = loadLift(lift, peopleWaiting)

  def loadLift(lift: Lift, peopleWaiting: List[Person]): Lift = {
    val peopleToLoad = peopleWaiting.filter(p => p.start == lift.location.floor)
    if (peopleToLoad.nonEmpty) {
      val nearestDestinationOfPeople = peopleToLoad.map(_.destination).minBy(d => difference(lift.location.floor, d))
      val nextDestination = if (lift.destination.nonEmpty && peopleToLoad.nonEmpty) {
        if (difference(lift.location.floor, lift.destination.get) < difference(lift.location.floor, nearestDestinationOfPeople)) {
          lift.destination
        } else Some(nearestDestinationOfPeople)
      } else Some(nearestDestinationOfPeople)
      lift.copy(people = lift.people ++ peopleToLoad, destination = nextDestination)
    } else lift
  }

  override def updateDestination(lift: Lift, peopleWaiting: List[Person]): Option[Int] =
    if (lift.people.nonEmpty) {
      println(s"people are ${lift.people}")
      Some(lift.people.map(_.destination).minBy(d => difference(lift.location.floor, d)))
    } else if (peopleWaiting.nonEmpty) {
      Some(peopleWaiting.map(_.start).minBy(d => difference(lift.location.floor, d)))
    }
    else None

  private def difference(location: Double, destination: Int): Double = (location - destination).abs
}
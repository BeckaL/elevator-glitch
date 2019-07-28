object BasicCriteria extends ScenarioCriteria {

  override def loadPeople(lifts: Lifts, peopleWaiting: People): List[Lift] = lifts.map(l => loadLift(l, peopleWaiting))

  def loadLift(lift: Lift, peopleWaiting: People): Lift = {
    val peopleToLoad = peopleWaiting.filter(p => p.start == lift.location)
    if (peopleToLoad.nonEmpty) {
      val nearestDestinationOfPeople = peopleToLoad.map(_.destination).minBy(d => difference(lift.location, d))
      val nextDestination = if (lift.destination.nonEmpty && peopleToLoad.nonEmpty) {
        if (difference(lift.location, lift.destination.get) < difference(lift.location, nearestDestinationOfPeople)) {
          lift.destination
        } else Some(nearestDestinationOfPeople)
      } else Some(nearestDestinationOfPeople)
      lift.copy(people = lift.people ++ peopleToLoad, destination = nextDestination)
    } else lift
  }

  private def difference(location: Double, destination: Int): Double = (location - destination).abs
}
package elevatorGlitch.elevatorGlitch.Criteria

import elevatorGlitch.Simulator.{ElevatorObject, Lift}

trait ScenarioCriteria extends ElevatorObject {

  def loadPeople(lift: Lift, peopleWaiting: People): Lift

  def updateDestination(lift: Lift, peopleWaiting: People): Option[Int]

}


























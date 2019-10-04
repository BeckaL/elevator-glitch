package elevatorGlitch.Simulator

import scala.util.Random

object RandomGenerator extends Randomiser {
  val r: Random.type = scala.util.Random

  def randomDestination(location: Int, floors: Int): Int = {
    val viableFloors = (0 until floors).toSet diff Set(location)
    viableFloors.toVector(r.nextInt(viableFloors.size))
  }

  def randomNumberOfPeople(): Int =
    r.nextInt(100) match {
      case x if x < 90 => 0
      case x if x < 97 => 1
      case _ => 2
    }
}

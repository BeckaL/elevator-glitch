import org.scalatest._

class SimulatorSpec extends FlatSpec with Matchers {

  object mockRandomiser extends Randomiser {
    override def randomDestination(location: Int, floors: Int): Int = 2
    override def randomNumberOfPeople(): Int = 1
  }
  "A simulator" should "be generated with a number of floors and lifts" in {

    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)
    s.floors shouldBe 5
    s.lifts shouldBe 2
  }

  "A simulator" should "be start with lifts on the ground floor with no destination or people, and no people waiting" in {

    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)
    val state = s.initialTick()
    state("lifts") shouldBe List(Lift(0, None, List()), Lift(0, None, List()))
    state("people") shouldBe List()
  }

  it should "generate new people with a destination and a location at a new tick" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)
    val people = s.generatePeople(List())
    assert(people.size == 6)
    assert(people(0).start == 0)
    assert(people(0).destination == 2)
  }
}
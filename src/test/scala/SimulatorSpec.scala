import org.scalatest._

class SimulatorSpec extends FlatSpec with Matchers {

  object mockRandomiser extends Randomiser {
    override def nextInt(i: Int): Int = i / 2
  }
  "A simulator" should "be generated with a number of floors and lifts" in {

    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)
    s.floors shouldBe 5
    s.lifts shouldBe 2
  }

  it should "generate new people with a destination and a location at a new tick" in {
    val s = new Simulator(floors = 5, lifts = 2, randomiser = mockRandomiser)
    val p = s.generatePeople(List())
    assert(p.size == 6)
    assert(p(0).start == 0)
    assert(p(0).destination == 2)
  }
}
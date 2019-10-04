import elevatorGlitch.Simulator.RandomGenerator

object Main extends App {
  def main(): Unit = {
//    SampleScenes.states.foreach(s => {
    ////      println(s.mkString("\n")); Thread.sleep(500)
    ////    })
    val r = new Runner(RandomGenerator, StdOutPrinter)

    r.run(maxTicks=50, sleep=150)
  }
  main()
}

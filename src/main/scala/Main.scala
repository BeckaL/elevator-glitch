import elevatorGlitch.Renderer.SampleScenes

object Main extends App {
  def main(): Unit = {
    SampleScenes.states.foreach(s => {
      println(s.mkString("\n")); Thread.sleep(500)
    })
  }
  main()
}

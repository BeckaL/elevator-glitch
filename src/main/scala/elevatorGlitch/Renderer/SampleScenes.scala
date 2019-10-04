package elevatorGlitch.Renderer

import elevatorGlitch.Renderer.SceneRenderer.{LiftToRender, PersonOnFloor, SceneToRender, createScene}

object SampleScenes {

  val s1: List[String] = createScene(SceneToRender(LiftToRender(0, 0, ""), List(PersonOnFloor(0)), List()))
  val s2: List[String] = createScene(SceneToRender(LiftToRender(0, 0, "left"), List(PersonOnFloor(0)), List()))
  val s3: List[String] = createScene(SceneToRender(LiftToRender(0, 1, "left"), List(), List()))
  val s4: List[String] = createScene(SceneToRender(LiftToRender(0, 1, ""), List(), List()))
  val s5: List[String] = createScene(SceneToRender(LiftToRender(1, 1, ""), List(), List()))
  val s6: List[String] = createScene(SceneToRender(LiftToRender(2, 1, ""), List(), List()))
  val s7: List[String] = createScene(SceneToRender(LiftToRender(3, 1, ""), List(), List()))
  val s8: List[String] = createScene(SceneToRender(LiftToRender(4, 1, ""), List(), List()))
  val s9: List[String] = createScene(SceneToRender(LiftToRender(5, 1, ""), List(), List()))
  val s10: List[String] = createScene(SceneToRender(LiftToRender(6, 1, ""), List(), List()))
  val s11: List[String] = createScene(SceneToRender(LiftToRender(7, 1, ""), List(), List()))
  val s12: List[String] = createScene(SceneToRender(LiftToRender(8, 1, ""), List(), List()))
  val s13: List[String] = createScene(SceneToRender(LiftToRender(9, 1, ""), List(), List()))
  val s14: List[String] = createScene(SceneToRender(LiftToRender(9, 1, "right"), List(), List()))
  val s15: List[String] = createScene(SceneToRender(LiftToRender(9, 0, "right"), List(), List(PersonOnFloor(1))))
  val s16: List[String] = createScene(SceneToRender(LiftToRender(9, 0, ""), List(), List(PersonOnFloor(1))))
  val states = List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16)

}

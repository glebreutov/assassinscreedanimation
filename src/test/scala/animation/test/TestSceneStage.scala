package animation.test

import animation.SceneState

object TestSceneStage extends App{

  (0 to 5).foreach((i) => println(SceneState(i)))
  (95 to 105).foreach((i) => println(SceneState(i)))
}

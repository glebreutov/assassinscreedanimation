package animation

case class SceneState(frameCounter: Int) {
  val frameCount = 100
  def stage: Int = frameCounter / frameCount
  def frame: Int = frameCounter % frameCount
  def angle: Double = {
    val d = if (stage % 2 == 0) frame * 60 / 100 else 60
    //val anotherOne = if((stage - 1) % 2 ==0) 60 else 0
    60 * (stage/2) + d
  }
  def next: SceneState = SceneState(frameCounter + 1)

  override def toString: String = {
    s"frameCounter: $frameCounter frame: $frame stage: $stage angle: $angle"
  }
}

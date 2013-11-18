package sample.tic_tac_toe.mvcobject

trait Controller extends ControllerForView with ControllerForModel

trait ControllerForView {
  def buttonClickedAt(row:Int, col:Int):Unit
}

trait ControllerForModel {
  def gridNotEmpty(row:Int, col:Int):Unit
  def playedCross(row:Int, col:Int):Unit
  def playedO(row:Int, col:Int):Unit
  def nextMove(move:Move):Unit
  def winner(move:Move):Unit
}

trait Model {
  def setController(controller:ControllerForModel): Unit
  def moveAt(row:Int, col:Int): Unit
}


trait Viewer {
  def setController(controller:ControllerForView): Unit
  def displyError(err:String): Unit
  def drawCross(row:Int, col:Int): Unit
  def drawO(row:Int, col:Int): Unit
  def displayNextMove(move:Move): Unit
  def announceWinner(winner:Move): Unit
}

sealed trait Move
final case object X extends Move
final case object O extends Move
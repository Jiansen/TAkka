package sample.tic_tac_toe.mvcobject


final class GameController(model:Model, view:Viewer) extends Controller{
  
  def buttonClickedAt(row:Int, col:Int):Unit = {
    model.moveAt(row, col)
  }
  def gridNotEmpty(row:Int, col:Int):Unit= {
    view.displyError("grid "+row+" , "+col+" is not empty")
  }  
  def playedCross(row:Int, col:Int):Unit= {
    view.drawCross(row, col)
  }
  def playedO(row:Int, col:Int):Unit= {
    view.drawO(row, col)
  }
  def nextMove(move:Move):Unit= {
    view.displayNextMove(move)
  }
  def winner(move:Move):Unit= {
    view.announceWinner(move)
  }
  
  model.setController(this)
  view.setController(this)
}
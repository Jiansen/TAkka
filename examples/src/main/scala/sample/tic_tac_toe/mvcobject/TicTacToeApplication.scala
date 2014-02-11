package sample.tic_tac_toe.mvcobject

object TicTacToeApplication extends App {
  val model = new GameModel;
  val view = new GameView;
  val controller = new GameController(model, view)  
}
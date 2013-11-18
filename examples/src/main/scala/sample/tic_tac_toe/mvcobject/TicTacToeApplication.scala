package sample.tic_tac_toe.mvcobject

object TicTacToeApplication extends App {
//  val system = ActorSystem("LocalTicTacToe")
//  val model = system.actorOf(Props[Controller2ModelMessage, Model], "model")
//  val view = system.actorOf(Props[Controller2ViewMessage, View], "view")
//  val controller = system.actorOf(Props(new Controller(model, view)), "controller")  
  val model = new GameModel;
  val view = new GameView;
  val controller = new GameController(model, view)
  
}
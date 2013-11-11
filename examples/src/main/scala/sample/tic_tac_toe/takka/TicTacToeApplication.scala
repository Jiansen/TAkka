package sample.tic_tac_toe.takka

import takka.actor._

object TikTakTokApplication extends App {
  val system = ActorSystem("LocalTikTakTok")
  val model = system.actorOf(Props[Controller2ModelMessage, Model], "model")
  val view = system.actorOf(Props[Controller2ViewMessage, View], "view")
  val controller = system.actorOf(Props(new Controller(model, view)), "controller")  
}
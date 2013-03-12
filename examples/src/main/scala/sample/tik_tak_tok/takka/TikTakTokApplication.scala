package sample.tik_tak_tok.takka

import takka.actor._

object TikTakTokApplication extends App {
  val system = ActorSystem("LocalTikTakTok")
  val model = system.actorOf(Props[Controller2ModelMessage, Model], "model")
  val viewer = system.actorOf(Props[Controller2ViewerMessage, Viewer], "viewer")
  val controller = system.actorOf(Props(new Controller(model, viewer)), "controller")  
}
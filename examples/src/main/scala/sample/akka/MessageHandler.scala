package sample.akka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging

class MessageHandler extends Actor {
  def receive = {
    case akka.actor.UnhandledMessage(message, sender, recipient) =>
      println("unhandled message:"+message);
  }
}
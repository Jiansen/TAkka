package sample.takka

class TAkkaServerActor extends takka.actor.TypedActor[String] {
  def typedReceive = {
    case m:String => println("received message: "+m)
  }
}

class MessageHandler(system: akka.actor.ActorSystem) extends akka.actor.Actor {
  def receive = {
    case akka.actor.UnhandledMessage(message, sender, recipient) =>
      println("unhandled message:"+message);
  }
}

object TAkkaInAkka extends App {
  val akkasystem = akka.actor.ActorSystem("AkkaSystem")
  val akkaserver = akkasystem.actorOf(akka.actor.Props[TAkkaServerActor], "server")

  val handler = akkasystem.actorOf(akka.actor.Props(new MessageHandler(akkasystem)))
  akkasystem.eventStream.subscribe(handler,classOf[akka.actor.UnhandledMessage]);
  akkaserver ! "Hello Akka"
  akkaserver ! 3
  
  val takkasystem = takka.actor.ActorSystem("TAkkaSystem")
  val takkaserver = takkasystem.actorOf(takka.actor.Props[String, ServerActor], "server")
  
  val untypedserver = takkaserver.untypedRef
  takkasystem.system.eventStream.subscribe(handler,classOf[akka.actor.UnhandledMessage]);
  
  untypedserver ! "Hello TAkka"
  untypedserver ! 4
}
/*
> sbt run

Enter number: [TAkkaInAkka]

Output:
received message: Hello Akka
unhandled message:3
received message: Hello TAkka
unhandled message:4

 */	
package sample.takka

class AkkaServerActor extends akka.actor.Actor {
  def receive = {
    case m:String => println("received message: "+m)
  }
}

object AkkaInTAkka extends App {
  val system = akka.actor.ActorSystem("AkkaSystem")
  val akkaserver = system.actorOf(akka.actor.Props[AkkaServerActor], "server")
  
  val takkaServer = new takka.actor.ActorRef[String]{
    val untypedRef = akkaserver
  }
  
  takkaServer ! "Hello World"
//  takkaServer ! 3
}
/*
> sbt run

Enter number: [TAkkaInAkka]

Output:
received message: Hello World
 */	
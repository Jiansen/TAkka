package sample.elevatorController.takka

object tracer extends EventHandler{
  /*
    Initializes the event handler.
  */
  override def init(arg:Any):EventHandler = {this}

  //Prints info on the event that has occured.
  override def handle_event(event:Event) = {//TODO:specify the type
    println(event)
  }
  
}
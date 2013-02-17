package sample.tik_tak_tok.takka

import takka.actor._

final class Model extends TypedActor[Controller2ModelMessage] {
  var controller:ActorRef[Model2ControllerMessage] = _
  
  def typedReceive = {
    case ModelsetController(control) => controller = control
    case ModelMove(row:Int, col:Int, state:Move) =>
      try{
        
      }
      model.setStatus(row, col, state)
      
  }
  
  
  private object model {
    sealed trait GridStatus
    case object Empty extends GridStatus
    case object XModelMove extends GridStatus
    case object OModelMove extends GridStatus // Uppercase O
    
    var nextMove:Boolean = true // true->X false->O
    
    val status:Array[Array[GridStatus]] = Array(Array(Empty, Empty, Empty),
                                                Array(Empty, Empty, Empty),
                                                Array(Empty, Empty, Empty))
    
    def setStatus(row:Int, col:Int, state:Move) = state match {
      case X =>
        if (status(row)(col) == Empty) {
          status(row)(col) = XModelMove
        }else{
          //TODO: exception
        }
      case O =>
        if (status(row)(col) == Empty) {
          status(row)(col) = OModelMove
        }else{
          //TODO: exception
        }
    }
                                                
  }
}


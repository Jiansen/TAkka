package sample.tic_tac_toe.mvcobject

final class GameModel extends Model {
  var controller:ControllerForModel = _
  
  def setController(controller:ControllerForModel): Unit = {
    this.controller = controller
  }
  
  def moveAt(row:Int, col:Int): Unit = {
    model.setStatus(row, col)  
  }  
  
  private object model {
    sealed trait GridStatus
    case object Empty extends GridStatus
    case object XModelMove extends GridStatus
    case object OModelMove extends GridStatus // Uppercase O
    
    var nextXMove:Boolean = true // true->X false->O
    
    val status:Array[Array[GridStatus]] = Array(Array(Empty, Empty, Empty),
                                                Array(Empty, Empty, Empty),
                                                Array(Empty, Empty, Empty))
    
    def setStatus(row:Int, col:Int) = {   
      if(nextXMove){
          if (status(row)(col) == Empty) {
            status(row)(col) = XModelMove
            controller.playedCross(row, col)
            nextXMove = false
            controller.nextMove(O)
          }else{
            controller.gridNotEmpty(row, col)
          }
      }else{
          if (status(row)(col) == Empty) {
            status(row)(col) = OModelMove
            controller.playedO(row, col)
            nextXMove = true
            controller.nextMove(X)
          }else{
            controller.gridNotEmpty(row, col)
          }     
      }

      checkWinner match {
        case Empty =>
        case XModelMove =>
          controller.winner(X)
        case OModelMove =>
          controller.winner(O)          
      }
    }
    // reuse GridStatus instead of a new set of values
   def checkWinner:GridStatus = {
     // check rows
     for (i<-0 until 3){
       if (status(i)(0) != Empty && status(i)(0) == status(i)(1) && status(i)(0) == status(i)(2)){
         return status(i)(0)
       }
     }
     // check cols
     for (i<-0 until 3){
       if (status(0)(i) != Empty && status(0)(i) == status(1)(i) && status(0)(i) == status(2)(i)){
         return status(0)(i)
       }
     }
     // check diagonal
     if(status(0)(0) != Empty && status(0)(0) ==  status(1)(1) &&  status(0)(0) ==  status(2)(2)){
       return status(0)(0) 
     }
     if(status(2)(0) != Empty && status(2)(0) ==  status(1)(1) &&  status(2)(0) ==  status(0)(2)){
       return status(2)(0) 
     }
     Empty
   }                                   
  }
}


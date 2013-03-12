package sample.tik_tak_tok.takka

import takka.actor._
import scala.swing._
import scala.swing.event._
import javax.swing.JOptionPane

final class Viewer extends TypedActor[Controller2ViewerMessage]{
   private var controller:ActorRef[Viewer2ControllerMessage] = _
   
   private var guiApp:GUIApplication = _;
      
   def typedReceive = {
     case ViewersetController(control) =>
       assert(controller == null, "controller has been set")
       controller = control
       guiApp = new GUIApplication(controller)
       guiApp.main(Array(""))              
     case DisplyError(err) =>
       guiApp.displayError(err)
     case DrawCross(row, col) =>
       guiApp.draw(row, col, true)
     case DrawO(row, col) =>
       guiApp.draw(row, col, false)
     case DisplayNextMove(move) =>
       guiApp.showNextMove(move)
     case AnnounceWinner(winner:Move) => winner match{
       case X => guiApp.announceWinner(true)
       case O => guiApp.announceWinner(false)
     }
       
   }
}


class GUIApplication(controller:ActorRef[Viewer2ControllerMessage]) extends SimpleSwingApplication {
  def draw(row:Int, col:Int, isCross:Boolean){
      if(isCross){
        grids(row)(col).text = "X"
      }else{
        grids(row)(col).text = "O" 
      }
  }
    
  def showNextMove(move:Move) = {
    NextMoveButton.showNextMove(move)
  }
  
  def displayError(err:String) {
      JOptionPane.showMessageDialog(null, err);
  }
  
  def announceWinner(isCross:Boolean) {
    if(isCross){
      JOptionPane.showMessageDialog(null, "X wins");
    }else{
      JOptionPane.showMessageDialog(null, "O wins");
    }
  }
    
  // grids on gameboard
  val grids:Array[Array[GameButton]] = Array.ofDim(3,3)
  
  protected class GameButton(val row:Int, val col:Int) extends Button
  protected object NextMoveButton extends Button {
    this.text = "X"
      
    def showNextMove(move:Move) = move match {
      case X => text = "X"
      case O => text = "O"
    }
  }
  
  def top = new MainFrame { 
    title = "Tik Tak Tok" 

    contents = new BorderPanel() {
      this.add(gameboard, BorderPanel.Position.Center)
      this.add(nextPanel, BorderPanel.Position.West)
      this.add(bugPanel, BorderPanel.Position.East)      
    }
    

    // game board
    object gameboard extends GridPanel(3,3) {
      for (i <- 0 until 3;
           j <- 0 until 3){
        val button = new GameButton(i,j)
        contents += button
        grids(i).update(j,button)
        this.listenTo(button)
      }

      reactions += {
        case ButtonClicked(b:GameButton) =>          
          // b.text = b.row.toString()
          controller ! ButtonClickedAt(b.row, b.col)
      }
    }
    
    // the next move
    object nextPanel extends BoxPanel(Orientation.Vertical){
      contents += NextMoveButton
    }

    // raise exceptions
    object bugPanel extends GridPanel(4,1){
      contents += new TextField(){
        text = "Destroy "
        editable = false
      }
      contents += new Button("Model")
      contents += new Button("Viewer")
      contents += new Button("Controller")
    }
  }
}
package sample.tik_tak_tok.takka

import takka.actor._
import scala.swing._
import scala.swing.event._

final class Viewer extends TypedActor[Controller2ViewerMessage]{
   var controller:ActorRef[Viewer2ControllerMessage] = _
   
   val guiApp:GUIApplication = new GUIApplication
      
   def typedReceive = {
     case ViewersetController(control) =>
       controller = control
       guiApp.main(Array(""))       
       
   }
}


class GUIApplication extends SimpleSwingApplication {
  def top = new MainFrame { 
    title = "Tik Tak Tok" 
    //size  = new Dimension(600,600) 
    
    // grids on gameboard
    val grids:Array[Array[GameButton]] = Array.ofDim(3,3)
    
    
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
          b.text = b.row.toString()
      }
      
      
      //contents(3).asInstanceOf[Button].text = "hi"
    }
    
    private class GameButton(val row:Int, val col:Int) extends Button {
      // text = row +" , "+ col
      // this.size = new Dimension(100,100)
    }
    
    // the next move
    object nextPanel extends BoxPanel(Orientation.Vertical){
      contents += new Button("X")
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
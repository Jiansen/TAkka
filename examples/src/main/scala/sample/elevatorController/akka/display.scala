package sample.elevatorController.akka

import akka.actor._
import scala.swing._
import takka.nameserver._
import javax.swing._
import scala.concurrent.ops._
import java.awt.Graphics2D
import akka.dispatch.{Future, Await}
import akka.util.duration._
import akka.pattern._

class ElevatorsMonitor(IFloor:Int, NFloors:Int, NElevs:Int) extends SimpleSwingApplication {
  assert(NFloors>=1, "Top level (currently "+NFloors+") must be greater or equal to 1")
  assert(IFloor>=1 && IFloor<=NFloors, "Initial Floor (currently "+IFloor+") must be between 1 and top level ("+NFloors+") (included)")
 
//  object display_elev extends SimpleSwingApplication {
	  // constants
	  val I = 10		//Pad
	  val BW = 50	//Quit button width
	  val BH = 30	//Quit button height
	  
	  val EW = 50	//Elevator width
	  val EH = 70	//Elevator height
	  val F = 10		//Thickness of line denoting limit between floors
	  val FBW = 100	//Floor button width
	  val FBH = 30	//Floor button height
	  val EBW = 45	//Elevator button width
	  val EBH = 25	//Elevator button height
	
	  // Compute some values needed later on
	  val CW = NElevs*EW + 2*F + (NElevs-1)*EW // Canvas width
	  val CH = NFloors*(EH+F)                  // Canvas height
	  
	  val WW = I+FBW+I+CW+I                         // Window width
	  val WH = I+CH+I+((NFloors + 1)/2)*EBH+I+BH+I  // Window height
	
	  //This panel holds the floor buttons
      private val fButtonPanel = new JPanel(null){// null laylout
	    this.setLocation(I, I)
	    this.setSize(FBW, CH)
	    this.setBorder(new javax.swing.border.LineBorder(java.awt.Color.green, 3))
	    this.setVisible(true)
	  }
      
      // This frame holds the elevator buttons
      val EButtonF = new JPanel(null){ // null layout
        this.setLocation(2*I+FBW, 2*I+CH)
        this.setSize(CW, ((NFloors+1)/2)*EBH)
        this.setBorder(new javax.swing.border.LineBorder(java.awt.Color.yellow, 3))
        this.setVisible(true)
      }
      
      // The canvas is used to draw the floors and elevators on
      private val canvas = new JPanel(null){// null laylout
	    this.setLocation(2*I+FBW, I)
	    this.setSize(CW, CH)
	    this.setBorder(new javax.swing.border.LineBorder(java.awt.Color.blue, 3))
	    this.setVisible(true)
	  }
      //gs:create(canvas, Win, [{x, 2*?I+?FBW}, {y, ?I},
	  //			     {width, CW}, {height, CH},
	  //			      {bw, 2}, {relief, raised}]),//TODO: what's this
      
      val pos:Int = CH - IFloor*(EH + F)
      var floors:List[(Floor, Int)] = draw_floors(NFloors, 0, canvas, CW, fButtonPanel)
      var elevGs:List[(Int,JPanel)] = draw_elevators(1, NElevs, NFloors, F, pos, canvas, CH, EButtonF)
            
	  //TODO: Quit Button
	  //display.start_e_graphics(1, pos, elevGs, floors, Nil)
      //display.start_e_graphics(pos, elevGs, floors)
	  
	  // Create the actual window
	  private val window = new JPanel(null) {
        this.setPreferredSize(new java.awt.Dimension(WW, WH))
        this.setLocation(0,0)
        add(fButtonPanel)
        add(canvas)
        add(EButtonF)
        this.setVisible(true)
	  }
	  
	  val top = new MainFrame {
	    title = "Simple Elevator Controller"
	    peer.add(window)
	  }
	  
	  //private functions
	  /*
	%%%--------------------------------------------------------------------------
	%%% draw_floors(F, Ybase, Canvas, CW, FButtonF) -> Floors
	%%%   F -> 1..6  Floor number, also recursion variable
	%%%   Ybase -> int()  Top Y coordinate for floor
	%%%   Canvas -> GS object, Canvas on which to draw the floors (= lines)
	%%%   CW -> int()  Canvas width in pixels
	%%%   FButtonF -> GS object, Frame in which to place the floor buttons
	%%%   Floors -> [{F,Y}]  List associating floors with pixel coordinates
	%%%
	%%% Recursive function for drawing the floors (= lines on the canvas) and
	%%% creating the floor buttons.
	%%%--------------------------------------------------------------------------
	   */
	  private def draw_floors(floor:Int, Ybase:Int, Canvas:javax.swing.JPanel, CW:Int, fButtonP:javax.swing.JPanel):List[(Floor, Int)] = {
	    //assert( floor >=0 && floor <=6 , "Floor number must between 1 to 6 (inclusive)")
	    if (floor==0) {Nil}
	    else {
	      // Compute where to draw the line and create it
	      // Ybase + floor height - half line width
	      val Yline = Ybase +EH//+ (EH+F) - F/2
	      val line = new JPanel(){
	        this.setLocation(0,Yline)
	        this.setSize(CW, F)
	        //this.setBorder(new javax.swing.border.LineBorder(java.awt.Color.black, 3))
	        this.setBackground(java.awt.Color.black)
	        this.setVisible(true)
	        Canvas.add(this)
	      }
	      //	    gs:create(line, Canvas, [{coords, [{0,Yline}, {CW,Yline}]},
		  //		     {width, ?F}]),// TODO:What's this
	
  	      // Compute where to place the button and create it
	      // Ybase + half floor height - half button height
	      val Ybutton = Ybase + (EH+F)/2 - FBH/2
	      val button = new JButton("Floor "+floor){
	        this.setLocation(0, Ybutton)
	        this.setSize(FBW, FBH)
	        this.setVisible(true)
	        fButtonP.add(this)
	      }
	      button.addActionListener(new java.awt.event.ActionListener(){
	          override def actionPerformed(e:java.awt.event.ActionEvent) {
	        	  	display.handle_info(Floor(floor))
              }
	      })
	      (Floor(floor), Ybase)::draw_floors(floor-1, Ybase+(EH+F), Canvas, CW, fButtonP)
	    }
	  }	  	  
/*
%%%--------------------------------------------------------------------------
%%% draw_elevators(E, Xbase, Y, Canvas, CH, EButtonF) -> ElevGs
%%%   E -> 1..3  Elevator number, also recursion variable
%%%   Xbase -> int()  Left X coordinate for elevator
%%%   Y -> int()  Top Y coordinate for elevators (same for all three)
%%%   Canvas -> GS object, Canvas on which to draw the elevators (= rects)
%%%   CH -> int()  Canvas height in pixels
%%%   EButtonF -> GS object, Frame in which to place the elevator buttons
%%%   ElevGs -> List of GS objects, the three elevators
%%%
%%% Recursive function for drawing the elevators (= rectangles on the canvas)
%%% and creating the elevator buttons.
%%%--------------------------------------------------------------------------
*/
      private def draw_elevators(E:Int, N:Int, NFloors:Int, Xbase:Int, Y:Int, Canvas:JPanel, CH:Int, EButtonF:JPanel):List[(Int,JPanel)] = {
        if (E>NElevs) {Nil}
        else{//Draw the elevator
          val elevGui = new JPanel(null) {
            this.setLocation(Xbase, Y)
            this.setSize(EW, EH)
            this.setBackground(java.awt.Color.black)
            this.setBorder(new javax.swing.border.LineBorder(java.awt.Color.black, 3))
            this.setVisible(true)
          }
          Canvas.add(elevGui)
          draw_buttons(1, E, NFloors, Xbase+EW/2, 0, EButtonF)
          (E,elevGui)::draw_elevators(E+1, N, NFloors, Xbase+2*EW, Y, Canvas, CH, EButtonF)
        }
      }
/*
%%%--------------------------------------------------------------------------
%%% draw_buttons(B, E, NFloors, X, Y, EButtonF)
%%%   B -> 1..NFloors  Button (floor) number, also recursion variable
%%%   E -> int()       Elevator number
%%%   NFloors -> int() Number of floors
%%%   X -> int()       Middle X coordinate
%%%   Y -> int()       Top Y coordinate
%%%   EButtonF ->      GS object, Frame in which to place the elevator buttons
%%%
%%% Recursive function for drawing the NFloors buttons for the elevator.
%%%--------------------------------------------------------------------------
*/
      private def draw_buttons(B:Int, E:Int, NFloors:Int, X:Int, Y:Int, EButtonF:JPanel):Unit = {
        if (B>NFloors) {Unit}
        else if (B % 2 != 0) {
          //Button with odd numbers are placed left of X
          val button = new JButton(B.toString()){
	        this.setLocation(X-EBW, Y)
	        this.setSize(EBW, EBH)
	        EButtonF.add(this)
	      }
	      button.addActionListener(new java.awt.event.ActionListener(){
	          override def actionPerformed(e:java.awt.event.ActionEvent) {
	        	  	display.handle_info((E,Floor(B)))
              }
	      })
	      draw_buttons(B+1, E, NFloors, X, Y, EButtonF)
        }
        else {
          //Buttons with even numbers are placed right of X
          val button = new JButton(B.toString()){
	        this.setLocation(X, Y)
	        this.setSize(EBW, EBH)
	        EButtonF.add(this)
	      }
	      button.addActionListener(new java.awt.event.ActionListener(){
	          override def actionPerformed(e:java.awt.event.ActionEvent) {
	        	  	display.handle_info((E,Floor(B)))
              }
	      })
          //Increase Y for next two buttons
          draw_buttons(B+1, E, NFloors, X, Y+EBH, EButtonF)
        }
      } 
//  }
  


}

object display extends EventHandler {
  private var system = ActorSystem("display") //when graphics is not supervised by g_sup
  
  private var gui:ElevatorsMonitor = _
  
  var ElevGs:List[(Int, ActorRef)] = _
  
  override def init(arg:Any):EventHandler = synchronized {
    arg match {
    case (iFloor:Int, nFloors:Int, nElevs:Int) =>
      if(gui==null) {//display handler has been initialised
        gui = new ElevatorsMonitor(iFloor, nFloors, nElevs)
        gui.startup(Array())
        ElevGs = start_e_graphics(gui.pos, gui.elevGs, gui.floors)
        this        
      }else{
        Thread.sleep(5000) // wait for another 5 seconds
        init(arg:Any)
      }
    }
  }
  
  def close() = synchronized {
    Thread.sleep(500)// In case gui has not been started.
    gui.top.close()    
    system.shutdown()
    system = ActorSystem("display")
    ElevGs = Nil
    gui = null
  }

  override def handle_event(event:Event) = event match {
    //sys_events: dispatches the interesting ones to the graphical elevator.
    case OpenEvent(eNo:Int) =>
      ElevGs.find(elevG =>elevG._1 == eNo) match {
        case Some(eg) => e_graphic.open(eg._2)
        case None => throw new Error("Internal Error: eNo "+eNo+" not found in ElevGs "+ElevGs+".")
      }
    case CloseEvent(eNo:Int) =>
      ElevGs.find(elevG =>elevG._1 == eNo) match {
        case Some(eg) => e_graphic.close(eg._2)
        case None => throw new Error("Internal Error: eNo "+eNo+" not found in ElevGs "+ElevGs+".")
      }      
    case MoveEvent(eNo:Int, dir:Direction) =>
      ElevGs.find(elevG =>elevG._1 == eNo) match {
        case Some(eg) => e_graphic.move(eg._2, dir)
        case None => throw new Error("Internal Error: eNo "+eNo+" not found in ElevGs "+ElevGs+".")
      }
    case StoppingEvent(eNo:Int) =>
      ElevGs.find(elevG =>elevG._1 == eNo) match {
        case Some(eg) => e_graphic.stop(eg._2)
        case None => throw new Error("Internal Error: eNo "+eNo+" not found in ElevGs "+ElevGs+".")
      }
    case ControllerStartedEvent(eNo:Int, ePid:akka.actor.ActorRef) =>
      ElevGs.find(elevG =>elevG._1 == eNo) match {
        case Some(eg) => e_graphic.set_controller(eg._2, ePid)
        case None => throw new Error("Internal Error: eNo "+eNo+" not found in ElevGs "+ElevGs+".")
      }
      
    //Events generated by the graphical elevator are ignored...
    case ApproachingEvent(eNo:Int, floor:Floor) => Unit
    case StoppedEvent(eNo:Int, floor:Floor) => Unit
    case PassingEvent(eNo:Int, floor:Floor) => Unit
    case ResetEvent(eNo:Int, state:ElevatorState, floor:Floor) => Unit
    //...and so are events generated by ourselves.
    case EButtonPressedEvent(eNo:Int, floor:Floor) => Unit
    case FButtonPressedEvent(floor:Floor) => Unit
  }

  def handle_info(inf:Any) = inf match {//TODO
    case floor:Floor => scheduler.f_button_pressed(floor)
    case (eNo:Int, floor:Floor) => scheduler.e_button_pressed(eNo, floor)
    case 'quit => //util.stop()
  }
  
  // method signature changed!!!
  // using loop to generate the result list, rather than using recursion.
  def start_e_graphics(pos:Int, elevGs:List[(Int,JPanel)], floors:List[(Floor, Int)]):List[(Int, ActorRef)] = {
    val n = elevGs.length
    var counter = 0
    for (eg<-elevGs) yield NameServer.get(TSymbol[ActorRef]('g_sup)) match {
       case None =>
         counter += 1
          //println("g_sup none")
         //(counter,system.actorOf(Props(e_graphic.start_link(pos, eg, floors)), "eg_"+counter))
         (counter,system.actorOf(Props(e_graphic.start_link(pos, eg, floors))))
       case Some(gSup) =>
         counter += 1
         val props = Props(e_graphic.start_link(pos, eg, floors))
         val res = ask(gSup, props)(200 millis).mapTo[ActorRef]
         (counter,Await.result(res, 200 millis))
    }    
  }
  //TODO: terminate and code_change
}
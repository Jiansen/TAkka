package sample.atm.takka

import takka.actor._
import scala.swing.event._


object webatm {
  val MODULE = 'webatm
  val SERVER_ADDR = "127,0,0,1"
  val HTTP_SERVER = "http://127.0.0.1"
  
  
//  private val system = ActorSystem("WebATMSystem")
  private var pri_actor:ActorRef[WebATMMsg] = _
  
  //Interface
  /*
  def start(name:Symbol, port:Int):ActorRef[WebATMMsg] = {
    /*
    Root = get_root(),
    Conf = [
        {port, Port},
        {server_name, atom_to_list(Name)},
        {bind_address, ?SERVER_ADDR},
        {server_root, filename:join(Root, "priv")}, % logs
        {document_root, filename:join(Root, "priv/www")}, % files
        {modules, [mod_alias, mod_auth, mod_esi, mod_actions,
                   mod_get, mod_head, mod_log, mod_trace]},
        {error_log, "logs/atm_error_log.txt"},
        {security_log, "logs/atm_security_log.txt"},
        {transfer_log, "logs/atm_transfer_log.txt"},
        {directory_index, ["atm.html"]},
        {erl_script_alias, {"/atm", [?MODULE]}}
    ],
    io:format("Visit ~s:~p to see the ATM. Javascript must be enabled.~n", [?HTTP_SERVER, Port]),
    {ok, Pid} = inets:start(httpd, Conf, stand_alone),
    make_name(Name, Port, undefined),
    {ok, Pid}.
    */
    //TODO
//    assert(system.system.actorFor("/user/webATM").isTerminated, "webATM has been started.")
    pri_actor = system.actorOf(Props(new webATM_Class(name, port)), "webATM")
    make_name(name, port, pri_actor)
    pri_actor
  }
*/
/*
  def start_link(name:Symbol, port:Int):Actor[WebATMMsg] = {
    //assert(pri_actor == null,  "webATM has been started.")
    val actor = new webATM_Class(name, port)
    pri_actor = actor.selfRef
    make_name(name, port, pri_actor)
    actor
  }
*/  
  def update_actor(act:ActorRef[WebATMMsg]) = {
    pri_actor = act
  }
  /*
  def stop() {
    val a = system.system.actorFor("/user/webATM")
    system.system.stop(a)
//    system.shutdown()
  }  
*/  
  //def do(name:Symbol, commands:List[ATMCommands]) {
  //  lookup_pid(name) ! {'#do', ListOfCmds}
  //}
    
  def make_name(name:Symbol, port:Int, pid:ActorRef[WebATMMsg]) {
    // try
    //    ets:new(?MODULE, [named_table, public, {heir, whereis(application_controller), permanent}])
    // catch
    //     error:badarg -> ok  % table exists
    // end,
    mydb.insert('webatm, (name, port, pid))
  }

  def display(text:String) = {
    pri_actor ! WebDisplay(text)
  }
    
  def append_line(text:String) = {//Not implemented
    pri_actor ! WebAppend(text)
  }
  
  def highlight(button:String) = {
    pri_actor ! WebHighLight(button)
  }
  
  def waitfor(duration:Int) = {
    Thread.sleep(duration)
  }
  
  def eject() = {
    display("Your card has been ejected.")
    Thread.sleep(1000)
    display("<html>Hello!<br>"+
               "welcome to the Erlang ATM machine!<br>"+
               "Please insert your card.<br></html>")
  }
 
  def hammerHit() = {
    pri_actor ! HammerHit
  }
  
  private def lookup_pid(name:Symbol):ActorRef[WebATMMsg] = {
    val tmp = mydb.matchWith('webatm, a => a match { case (sname, _, _) => name == sname })
    tmp match {
      case (_, _, actor:ActorRef[WebATMMsg])::Nil => actor
      case _ => throw new Error("ATM Internal Error: webatm.scala L103")
    }
  }
  
  private def update_pid(port:Int, pid:ActorRef[WebATMMsg]) = {
    val tmp = mydb.updateWith('webatm, a => a match { 
//      case (name:Symbol, sport:Int, actor:ActorRef[WebATMMsg]) => 
      case (name:Symbol, sport:Int, actor) =>     
        if (port == sport) (name, port, pid) else (name, sport, actor)})
  }
      
  private def delete_name(tab:Symbol) {
    mydb.delete(tab)
  }
  
  
  
}

sealed trait WebATMMsg
case class WebDisplay(text:String) extends WebATMMsg
case class WebAppend(text:String) extends WebATMMsg
case class WebHighLight(button:String) extends WebATMMsg

case object HammerHit extends WebATMMsg

class webATM_Class(name:Symbol, port:Int) extends Actor[WebATMMsg]{
  def typedReceive = {
    case HammerHit =>
      throw new Exception("Terminal is hit by a hammer")
    case WebDisplay(text) => 
      guiATM.ScreenPanel.screen.text = text
    case WebAppend(text) => //Not implemented
      Unit
    case WebHighLight(button) => button match {
      case "withdraw" => guiATM.withdrawButton.background = java.awt.Color.BLUE
      case "statement" => guiATM.statementButton.background = java.awt.Color.BLUE
      case "balance" => guiATM.balanceButton.background = java.awt.Color.BLUE
      case "off" =>
        guiATM.withdrawButton.background = null//java.awt.Color.WHITE
        guiATM.statementButton.background = null//java.awt.Color.WHITE
        guiATM.balanceButton.background = null//java.awt.Color.WHITE
    } 
  }
  
  import scala.swing._
  
  object guiATM extends SimpleSwingApplication {
    val frameWidth = 600
    val frameHight = 400
    
    val withdrawButton = new Button { text = "Withdraw Cash" }
    val balanceButton = new Button { text = "Balance Enquiry" } 
    val statementButton = new Button { text = "Mini Statement" }
    
    // a hammer hit the ATM terminal
    val hammerButton = new Button("Hammer")
    
    val OptionPanel = new BoxPanel(Orientation.Vertical){
      contents += withdrawButton
      contents += balanceButton   
      contents += statementButton 
      
      contents += hammerButton
    }
    
    //Fetches cards
    val accountsList = backend.accountAll//.map(a => (a.no, a.name))
    val accounts = new ListView(accountsList)
    
    val card_insert_button = new Button { text = "Insert Card"}
    
    val ScreenPanel = new BorderPanel(){
      val screen = new Label() {
        foreground = (java.awt.Color.black) 
        visible = true
        text = "<html>Hello!<br>"+
               "welcome to the Erlang ATM machine!<br>"+
               "Please insert your card.<br></html>"
      }
      
      add(screen, BorderPanel.Position.Center)
      
      val accountPanel = new BorderPanel(){
        add(accounts, BorderPanel.Position.Center)
        add(card_insert_button, BorderPanel.Position.South)
      }
      add(accountPanel, BorderPanel.Position.South)
      
      //size = new java.awt.Dimension(400, 200) 
    }
    
    val enterButton = new Button {text = "Enter"}
    val cancelButton = new Button {text = "Cancel"}
    val clearButton = new Button {text = "Clear"}
    
    val button1 = new Button {text = "1"}
    val button2 = new Button {text = "2"}
    val button3 = new Button {text = "3"}
    val button4 = new Button {text = "4"}
    val button5 = new Button {text = "5"}
    val button6 = new Button {text = "6"}
    val button7 = new Button {text = "7"}
    val button8 = new Button {text = "8"}
    val button9 = new Button {text = "9"}
    val button0 = new Button {text = "0"}
    
    val ButtonPanel = new GridPanel(5,3){
      contents += enterButton
      contents += cancelButton
      contents += clearButton
      contents += button1
      contents += button2
      contents += button3
      contents += button4
      contents += button5
      contents += button6
      contents += button7
      contents += button8
      contents += button9
      contents += button0
    }
    
    val top = new MainFrame {
      title = "ATM Machine"
         
      contents = new BorderPanel(){
        add(OptionPanel, BorderPanel.Position.West)
        add(ScreenPanel, BorderPanel.Position.Center)
        add(ButtonPanel, BorderPanel.Position.East)
      }
      size = new java.awt.Dimension(frameWidth, frameHight) 
    }
    
    listenTo(hammerButton)
    
    listenTo(withdrawButton)
    listenTo(balanceButton)
    listenTo(statementButton)
    
    listenTo(enterButton)
    listenTo(cancelButton)
    listenTo(clearButton)
    
    listenTo(button1)
    listenTo(button2)
    listenTo(button3)
    listenTo(button4)
    listenTo(button5)
    listenTo(button6)
    listenTo(button7)
    listenTo(button8)
    listenTo(button9)
    listenTo(button0)
    
    //listenTo(accounts.selection)
    listenTo(card_insert_button)
    reactions += {
      case ButtonClicked(b) => b.text match {//User pressed a button
        case "Hammer" =>
          webatm.hammerHit()
          //throw new Exception("ATM terminal is hitted by a Hammer")
        
        case "Withdraw Cash" => atm.event(SelectionWithdraw)
        case "Balance Enquiry" => atm.event(SelectionBalance)
        case "Mini Statement" => atm.event(SelectionStatement)
        case "Enter" => atm.event(Enter)
        case "Cancel" => atm.event(Cancel)
        case "Clear" => atm.event(Clear)
        case "Insert Card" => 
          val no = accounts.selection.items(0)._1.asInstanceOf[Int]
          atm.card_inserted(no)
        case i => //digit button pressed 
          // ScreenPanel.screen.text = "button clicks: "+ b.text          
          atm.event(Digit(i.toCharArray()(0)))
      }
      //case SelectionChanged(`accounts`) => //User picked a card
      //  val no = accounts.selection.items(0)._1.asInstanceOf[Int]        
      //  atm.card_inserted(no)
      case e => Unit //println(e)

    }
  }
  
  override def preStart() = {
    guiATM.startup(Array())
    webatm.update_actor(typedSelf)
    println("ATM terminal started")    
  }
  
  override def postStop() = {
    guiATM.shutdown()
//    webatm.stop()
  }
}

/*
object test extends App{
  backend.start_link()
  webatm.start_link('Hello, 8080)
}
*/
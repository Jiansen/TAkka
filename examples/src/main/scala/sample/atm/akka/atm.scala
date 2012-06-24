package sample.atm.akka

import akka.actor._
import akka.util.duration._

sealed trait ATMState
case object IdleState extends ATMState
case object GetPinState extends ATMState
case object SelectionState extends ATMState
case object WithdrawState extends ATMState
case object TimeoutState extends ATMState

sealed trait ATMEvent


case class ATMStateData(name:String, accountNo:Int, input:String, pin:String)//TODO:type of input

object atm {
/*
-export([start/1, start_link/1, stop/1, card_inserted/2, event/2]).

-export([init/1, idle/2, get_pin/2, selection/2, withdraw/2, timeout/2,
         handle_event/3, idle/3, get_pin/3, selection/3, withdraw/3, timeout/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {name, accountNo, input = [], pin}).
 */  
  private val system = ActorSystem("ATMSystem")
  private var pri_actor:ActorRef = _
  
  //Interface
  def start(Name:Symbol):ActorRef = {
    assert(system.actorFor("/user/atm").isTerminated, "ATM has been started.")
    pri_actor = system.actorOf(Props(new ATM_Class()), "atm")
    pri_actor
  }
  
  def start_link(Name:Symbol):Actor = {
    if (pri_actor != null) {
      println(pri_actor)
    }
    //assert(pri_actor == null,  "ATM has been started.")
    val actor = new ATM_Class()
    pri_actor = actor.self
    actor
  }
  
  def update_actor(act:ActorRef) = {
    pri_actor = act
  }
  
  def stop() {// only when it is a top-level actor
      val a = system.actorFor("/user/atm")
      system.stop(a)
      system.shutdown()
  }

  def card_inserted(account:Int) = { pri_actor ! ('card_inserted, account) }
  
  def event(e:Any) = {//TODO: Not type safe
    pri_actor ! e
  }
  
}

class ATM_Class extends Actor with FSM[ATMState, ATMStateData] {
   import FSM._
   
   //constant
   val TIME_LIMIT = 10000.millis
   
   startWith(IdleState, ATMStateData("", 0, "", ""))
   
   override def preStart() = {
     atm.update_actor(self)
   }
  
   override def postStop() = {
     atm.stop()
   }
   
   when(IdleState) {
     case Event(('card_inserted, accountNumber:Int), state) =>
       webatm.display("Please type your PIN code")       
       goto(GetPinState) using ATMStateData(state.name, accountNumber, state.input, state.pin) forMax TIME_LIMIT
     case Event('clear, state) => clear(IdleState, state)
     case Event('cancel, state) => cancel(state)
     case Event(('digit, _), state) => stay using state
     case Event(('selection, _), state) => stay using state
     case Event('enter, state) => stay using state
     case Event(StateTimeout, state) => stay using state
     case Event('stop, state) => stop using state
   }
   
   when(GetPinState) {
     case Event('clear, state) => clear(GetPinState, state)
     case Event('cancel, state) => cancel(state)
     case Event(('digit, digit:Char), state) =>
       val digits = state.input + digit
       webatm.display(digits)
       goto(GetPinState) using ATMStateData(state.name, state.accountNo, digits, state.pin) forMax TIME_LIMIT
     case Event('enter, state) =>
       if (backend.pin_valid(state.accountNo, state.input)){
         webatm.display("Please make your selection")
         goto(SelectionState) using ATMStateData(state.name, state.accountNo, "", state.input) forMax TIME_LIMIT
       }else{
         webatm.display("<HTML>PIN code incorrect!<br>Please try again.</HTML>")
         goto(GetPinState) using ATMStateData(state.name, state.accountNo, "", state.pin) forMax TIME_LIMIT
       }
       case Event(('selection, _), state) => stay using state forMax TIME_LIMIT
       case Event(('card_inserted, _accountNumber:Int), state) => stay using state forMax TIME_LIMIT
       case Event(StateTimeout,state) => goto(TimeoutState) using state
       case Event('stop, state) => stop using state
   }

   when(SelectionState) {
     case Event('clear, state) => clear(GetPinState, state)
     case Event('cancel, state) => cancel(state)
     case Event(('selection, 'withdraw), state) =>
       webatm.highlight("off")
       webatm.highlight("withdraw")
       webatm.display("How much would you like to withdraw?")
       goto(WithdrawState) using state forMax TIME_LIMIT
     case Event(('selection, 'balance), state) =>
       webatm.highlight("off")
       webatm.highlight("balance")
       balance(state)
       stay using state forMax TIME_LIMIT
     case Event(('selection, 'statement), state) =>
       webatm.highlight("off")
       webatm.highlight("statement")
       mini_statement(state)
       stay using state forMax TIME_LIMIT
     case Event(('digit, _), state) =>
       stay using state forMax TIME_LIMIT
     case Event('enter, state) =>
       stay using state forMax TIME_LIMIT
     case Event(('card_inserted, _accountNumber:Int), state) => stay using state forMax TIME_LIMIT
     case Event(StateTimeout, state) => goto(TimeoutState) using state
     case Event('stop, state) => stop using state
   }

   when(WithdrawState){
     case Event('clear, state) => clear(GetPinState, state)
     case Event('cancel, state) => cancel(state)
     case Event(('digit, digit:Char), state) =>
       val input = state.input + digit
       webatm.display(input)
       stay using ATMStateData(state.name, state.accountNo, input, state.pin) forMax TIME_LIMIT 
     case Event('enter, state) => 
       backend.withdraw(state.accountNo, state.pin, state.input.toInt) match {
         case 'OK =>
           webatm.display("Take the money and run.")
           webatm.waitfor(3500)
           webatm.highlight("off")
           webatm.eject()
           backend.eject(state.accountNo)
           Thread.sleep(3500)
         case e:Error =>
           println("fail: "+e)
           webatm.display(
               "<html>Could not withdraw money!<br>"+
               e.getCause()+"<br></html>"
           )
           webatm.waitfor(3500)
           webatm.highlight("off")
           webatm.eject()
           
           backend.eject(state.accountNo)
           Thread.sleep(3500)
       }
       goto(IdleState) using ATMStateData(state.name, 0, "", "")
     case Event(('selection, _), state) => stay using state forMax TIME_LIMIT
     case Event(('card_inserted, _accountNumber:Int), state) => stay using state forMax TIME_LIMIT
     case Event(StateTimeout, state) => goto(TimeoutState) using state
     case Event('stop, state) => stop using state
   }
   
   when(TimeoutState) {     
     case Event(_, state) =>
       webatm.display("Session Timed Out.")
       Thread.sleep(1000)
       webatm.eject()
       backend.eject(state.accountNo)
       goto(IdleState) using ATMStateData(state.name, 0, "", "")
   }
   
   private def clear(name:ATMState, state:ATMStateData):ATM_Class.this.State = {
     webatm.display(" ")
     name match {
       case IdleState => goto(IdleState) using ATMStateData(state.name, state.accountNo, "", state.pin) // no time limit on idle
       case _name => stay using ATMStateData(state.name, state.accountNo, "", state.pin) forMax TIME_LIMIT    
     }
   }

   private def cancel(state:ATMStateData):ATM_Class.this.State = {
    webatm.display("cancel: Cancel button pressed")
    Thread.sleep(500)
    webatm.eject()
    backend.eject(state.accountNo)
    goto(IdleState) using ATMStateData(state.name, 0, "", "")
   }

   // Bug in Erlang version: backend.balance may return error
   private def balance(state:ATMStateData) = {
     val balance = backend.balance(state.accountNo, state.pin) match {
       case e:Error =>
         throw e
       case b:Int =>  
         webatm.display(
           "<html>Balance:<br>"+
           "-------------------------<br>"+
           "£ " + b+"<br></html>" 
         )
     }    
   }
   
   // Bug in Erlang version: backend.transactions may return error
   private def mini_statement(state:ATMStateData) = {
     val trs = backend.transactions(state.accountNo, state.pin) match {
       case e:Error =>
         throw e
       case ts:List[Transaction] => ts
     }
     val balance = backend.balance(state.accountNo, state.pin)//pin has been checked
     val trs1 = select10(trs, Nil, 9)
     
     var statementHTML = "<html>Mini Statement:<br>---------------------<br>"
     for (trs <- trs1) trs match {
       case Transaction(trans_type, date, amount) =>
         val con = trans_type match{
           case DepositType => ""
           case WithdrawType => "-"
         }
         statementHTML += date.getDate() +"-"+ (date.getMonth()+1) +"-"+ (date.getYear()+1900) +": "+ con +" "+ amount +"<br>"
     }
     statementHTML += "Balance: £ " + balance +"<br></html>"
     webatm.display(statementHTML)
   }
   
   // the first 10 transactions in reverse order
   private def select10(trs:List[Transaction], acc:List[Transaction], n:Int):List[Transaction] = (trs,acc, n) match {
     case (Nil, acc, _) => acc
     case (_, acc, 0) => acc
     case (h::t, acc, n) => select10(t, h::acc, n-1)     
   }
   
}


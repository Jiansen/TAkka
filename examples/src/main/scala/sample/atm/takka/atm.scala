package sample.atm.takka

import takka.actor._
import akka.util.duration._
import java.util.Calendar

sealed trait ATMState
case object IdleState extends ATMState
case object GetPinState extends ATMState
case object SelectionState extends ATMState
case object WithdrawState extends ATMState
//case object TimeoutState extends ATMState

sealed trait ATMEvent
case class CardInserted(accountNumber:Int) extends ATMEvent
case object Clear extends ATMEvent
case object Cancel extends ATMEvent
case class Digit(digit:Char) extends ATMEvent
case object SelectionWithdraw extends ATMEvent
case object SelectionBalance extends ATMEvent
case object SelectionStatement extends ATMEvent
case object Enter extends ATMEvent
case object StateTimeout extends ATMEvent
case object Stop extends ATMEvent

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
  private var pri_actor:ActorRef[ATMEvent] = _
  
  //Interface
  def start(Name:Symbol):ActorRef[ATMEvent] = {
    //we don't care the type of "/user/atm" here
    assert(system.actorFor("/user/atm").isTerminated, "ATM has been started.")
    pri_actor = system.actorOf(Props(new ATM_Class()), "atm")
    pri_actor
  }
  
  def start_link(Name:Symbol):Actor[ATMEvent] = {
    if (pri_actor != null) {
      println(pri_actor)
    }
    //assert(pri_actor == null,  "ATM has been started.")
    val actor = new ATM_Class()
    pri_actor = actor.typedSelf
    actor
  }
  
  def update_actor(act:ActorRef[ATMEvent]) = {
    pri_actor = act
  }
  
  def stop() {// type for "user/atm" does not matter at this place
      val a = system.actorFor("/user/atm")
      system.stop(a)
      system.shutdown()
  }

  def card_inserted(account:Int) = { pri_actor ! CardInserted(account) }
  
  def event(e:ATMEvent) = {//TODO: Not type safe
    pri_actor ! e
  }
  
}

class ATM_Class extends Actor[ATMEvent] with FSM[ATMState, ATMStateData, ATMEvent] {
   import FSM._
   
   //constant
   val TIME_LIMIT = 10000.millis
   
   startWith(IdleState, ATMStateData("", 0, "", ""))
   
   override def preStart() = {
     atm.update_actor(typedSelf)
     println("ATM started: "+typedSelf)
   }
  
   override def postStop() = {
     atm.stop()
   }
   
   when(IdleState) {
     case Event(CardInserted(accountNumber), state) =>
       webatm.display("Please type your PIN code")       
       goto(GetPinState) using ATMStateData(state.name, accountNumber, state.input, state.pin) forMax TIME_LIMIT
     case Event(Clear, state) => clear(IdleState, state)
     case Event(Cancel, state) => cancel(state)
     case Event(Digit(_), state) => stay using state
     case Event(SelectionWithdraw, state) => stay using state
     case Event(SelectionBalance, state) => stay using state
     case Event(SelectionStatement, state) => stay using state
     case Event(Enter, state) => stay using state
     case Event(Stop, state) => stop using state

//     case Event(StateTimeout, state) => stay using state     
     case StateTimeout(state) => stay using state
   }
   
   when(GetPinState) {
     case Event(Clear, state) => clear(GetPinState, state)
     case Event(Cancel, state) => cancel(state)
     case Event(Digit(digit), state) =>
       val digits = state.input + digit
       webatm.display(digits)
       goto(GetPinState) using ATMStateData(state.name, state.accountNo, digits, state.pin) forMax TIME_LIMIT
     case Event(Enter, state) =>
       if (backend.pin_valid(state.accountNo, state.input)){
         webatm.display("Please make your selection")
         goto(SelectionState) using ATMStateData(state.name, state.accountNo, "", state.input) forMax TIME_LIMIT
       }else{
         webatm.display("<HTML>PIN code incorrect!<br>Please try again.</HTML>")
         goto(GetPinState) using ATMStateData(state.name, state.accountNo, "", state.pin) forMax TIME_LIMIT
       }
     case Event(SelectionWithdraw, state) => stay using state forMax TIME_LIMIT
     case Event(SelectionBalance, state) => stay using state forMax TIME_LIMIT
     case Event(SelectionStatement, state) => stay using state forMax TIME_LIMIT
     case Event(CardInserted(_accountNumber), state) => stay using state forMax TIME_LIMIT
     case StateTimeout(state) =>
       webatm.display("Session Timed Out.")
       webatm.highlight("off")
       Thread.sleep(1000)
       webatm.eject()
       backend.eject(currentState.stateData.accountNo)
       goto(IdleState) using ATMStateData(currentState.stateData.name, 0, "", "")
     case Event(Stop, state) => stop using state
   }

   when(SelectionState) {
     case Event(Clear, state) => clear(GetPinState, state)
     case Event(Cancel, state) => cancel(state)
     case Event(SelectionWithdraw, state) =>
       webatm.highlight("off")
       webatm.highlight("withdraw")
       webatm.display("How much would you like to withdraw?")
       goto(WithdrawState) using state forMax TIME_LIMIT
     case Event(SelectionBalance, state) =>
       webatm.highlight("off")
       webatm.highlight("balance")
       balance(state)
       stay using state forMax TIME_LIMIT
     case Event(SelectionStatement, state) =>
       webatm.highlight("off")
       webatm.highlight("statement")
       mini_statement(state)
       stay using state forMax TIME_LIMIT
     case Event(Digit(_), state) =>
       stay using state forMax TIME_LIMIT
     case Event(Enter, state) =>
       stay using state forMax TIME_LIMIT
     case Event(CardInserted(_accountNumber), state) => stay using state forMax TIME_LIMIT
     case StateTimeout(state) => 
       webatm.display("Session Timed Out.")
       webatm.highlight("off")
       Thread.sleep(1000)
       webatm.eject()
       backend.eject(currentState.stateData.accountNo)
       goto(IdleState) using ATMStateData(currentState.stateData.name, 0, "", "")
     case Event(Stop, state) => stop using state
   }

   when(WithdrawState){
     case Event(Clear, state) => clear(GetPinState, state)
     case Event(Cancel, state) => cancel(state)
     case Event(Digit(digit), state) =>
       val input = state.input + digit
       webatm.display(input)
       stay using ATMStateData(state.name, state.accountNo, input, state.pin) forMax TIME_LIMIT 
     case Event(Enter, state) => 
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
     case Event(SelectionWithdraw, state) => stay using state forMax TIME_LIMIT
     case Event(SelectionBalance, state) => stay using state forMax TIME_LIMIT
     case Event(SelectionStatement, state) => stay using state forMax TIME_LIMIT
     case Event(CardInserted(_accountNumber), state) => stay using state forMax TIME_LIMIT
     case StateTimeout(state) => 
       webatm.display("Session Timed Out.")
       webatm.highlight("off")
       Thread.sleep(1000)
       webatm.eject()
       backend.eject(currentState.stateData.accountNo)
       goto(IdleState) using ATMStateData(currentState.stateData.name, 0, "", "")
     case Event(Stop, state) => stop using state
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
         statementHTML += date.get(Calendar.DAY_OF_MONTH) +"-"+ date.get(Calendar.MONTH) +"-"+ date.get(Calendar.YEAR) +": "+ con +" "+ amount +"<br>"
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


package sample.atm.akka

import akka.actor._
import java.util.Date
import akka.util.{Timeout, Duration, FiniteDuration}
import akka.dispatch.{Future, Await}
import akka.util.duration._
import akka.pattern.ask

class backend extends Actor{
  // constants
  val DB = db_list
  val ACCOUNTS = List(new Account(1, 100, "1234", "Henry Nystrom", Nil),
                      new Account(2, 200, "4321", "Fransceco Cesarini", Nil),
                      new Account(3, 1000, "1111", "Donald Duck", Nil),
                      new Account(4, 5000, "1234", "Henry Nystrom", Nil)
                      )

//-record(state, {accounts, blocked = [], alarms = []}).
  case class STATE(accounts:List[Account], blocked:List[Int], alarms:List[Int])
  
  var state:STATE = _ 
  // GenServer part                      
  //def init() = {
  override def preStart() {
    //process_flag(trap_exit, true)
    val accounts = ACCOUNTS.foldLeft(DB.empty())((db, account) => DB.insert(account, db))
    state = STATE(accounts, Nil, Nil)
    backend.update_actor(self)
  }
  
  def handle_call(request:BackendMsg, state:STATE):(Any, STATE) = request match {
    case AccountNo(account) => (findAccount(account, state), state)
    case AccountName(name) => (findAccounts(name, state), state)
    case AccountAll => 
      val reply = state.accounts.map(acc => (acc.no, acc.name))
      (reply, state)
    case IsPinValid(accNo, pin) => (state.blocked.contains(accNo), state.alarms.contains(accNo)) match {
      case (true, true)  => (true, state) 
      case (true, false) => 
        //alarm_handler:set_alarm({AccountNumber, Pid}),//TODO
        stats.logging(('set_alarm, accNo, sender).toString)
        (true, STATE(state.accounts, state.blocked, accNo::state.alarms))
      case (false, _)    =>  
        val account = findAccount(accNo, state)
        val bool = isPinValid(account, pin)
        stats.logging(('login, account, bool).toString)
        (bool, state)
    }
    case NewAccount(balance, pin, name) =>
      val accounts = state.accounts
      val no = DB.db_size(accounts) 
      val newaccs = DB.insert(new_account(no, balance, pin, name), accounts) //TODO: bug? new account number?
      ('OK, STATE(newaccs, state.blocked, state.alarms))
    case Balance(accNo, pin) =>
      if(state.blocked.contains(accNo)){
        (0, state)
      }else{
        (balance(accNo, pin, state), state)
      }
    case Transactions(accNo, pin) =>
      if(state.blocked.contains(accNo)){
        (Nil, state)
      }else{
        (transactions(accNo, pin, state), state)
      }
    case Withdraw(from, pin, amount) =>
      if( state.blocked.contains(from) ){
        (new Error("Not enough money on account!"), state)
      }else{
        try{
          val newstate = withdraw(from, pin, amount, state)
          stats.logging(('withdraw, from, amount).toString)
          ('OK, newstate)
        }catch{
          case e:Error => (e, state)
        }
      }
    case Deposit(to, amount) =>
      try{
        val newstate = deposit(to, amount, state)
        stats.logging(('deposit, to, amount).toString)
        ('OK, newstate)
      }catch{
        case e:Error => (e, state)
      }
    case Transfer(from, to, pin, amount) =>
      try{
        val newstate = transfer(from, to, pin, amount, state)
        stats.logging(('transfer, from, to, amount).toString)
        ('OK, newstate)
      }catch{
        case e:Error => (e, state)
      }
    case ChangePin(accountNo, oldPin, newPin) =>
      try{
        val newstate = changePin(accountNo, oldPin, newPin, state)
        ('OK, newstate)
      }catch{
        case e:Error => (e, state)
      }
    case Block(accountNo) => ('OK, block(accountNo, state))
    case Eject(accountNo) => 
      stats.logging(('eject, accountNo).toString)
      ('OK, eject(accountNo, state))
  }
  
  //def handle_cast(a:Int, b:Int) {}
  
  //def handle_info(a:Int, b:Int) {}
  
  //def terminate(a:Int, b:Int) {}
  
  //def code_change(a:Int, b:Int, c:Int) {}  
  
  
  def receive = {
    case request: BackendMsg => handle_call(request, state) match {
      case (reply, newstate) =>
        state = newstate
        sender ! reply
    }
  }

  
  // Internal Functions
  private def new_account(No:Int, Balance:Int, Pin:String, Name:String) : Account = {
    new Account(No, Balance, Pin, Name, Nil)
  }
  
  private def findAccount(accountN:Int, state:STATE) : Account = {
    DB.lookup(accountN, state.accounts) match {
      case Some(acc) => acc
      case None => throw new Exception("Account "+accountN+" not found!")
    }
  }
  
  private def findAccounts(name:String, state:STATE) : List[Account] = {
    DB.lookup_all(name, 'name, state.accounts)
  }

  private def withdraw(accountNo:Int, pin:String, amount:Int, state:STATE):STATE = {
    val account = findAccount(accountNo, state)
    if (isPinValid(account, pin)){
      if (account.balance < amount){
        throw new Error("Not enough money on account!")
      }else{
        val newBalance = account.balance - amount
        val newtransactions = Transaction(WithdrawType, new Date(), amount)::account.transactions
        account.balance = newBalance
        account.transactions = newtransactions
        
        state
      }
    }else{
      throw new Error("PIN code not valid!") 
    }
  }

  private def deposit(accountNo:Int, amount:Int, state:STATE):STATE = {
    val account = findAccount(accountNo, state)
    val newBalance = account.balance + amount
    val newtransactions = Transaction(DepositType, new Date(), amount) :: account.transactions
    account.balance = newBalance
    account.transactions = newtransactions
    state
  }

  private def balance(accountNo:Int, pin:String, state:STATE):Int = {
    val account = findAccount(accountNo, state)
    if (isPinValid(account, pin)){
      account.balance
    }else{
      throw new Error("PIN code not valid!") 
    }
  }

  private def transactions(accountNo:Int, pin:String, state:STATE):List[Transaction] = {
    val account = findAccount(accountNo, state)
    if (isPinValid(account, pin)){
      account.transactions
    }else{
      throw new Error("PIN code not valid!") 
    }
  }
  
  private def transfer(fromAccountNo:Int, toAccountNo:Int, pin:String, amount:Int, state:STATE):STATE = {
    var newstate:STATE = state
    try {
      newstate = withdraw(fromAccountNo, pin, amount, state)
    }catch{
      case e:Error => throw e
    }
    deposit(toAccountNo, amount, newstate)
  }
  
/*
@spec is_pin_valid(account() | [account], pin()) -> bool()
is_pin_valid([], _) -> false;
is_pin_valid([Account | _], Pin) -> Account#account.pin == Pin;
is_pin_valid(Account, Pin) -> Account#account.pin == Pin.
 */  
  def isPinValid(account:Account, pin:String):Boolean = {
    account.pin == pin
  }

  // TODO: Bug in Erlang version
  // Change pin for all users with the same name if the pin is valid for the first one !!!
  private def changePin(accountNo:Int, oldPin:String, newPin:String, state:STATE):STATE = {
    val account = findAccount(accountNo, state)
    if (isPinValid(account, oldPin)){
      account.pin = newPin
    }
    state
  }
  
  private def block(accountNo:Int, state:STATE):STATE = {
    STATE(state.accounts, accountNo::(state.blocked), state.alarms)
  }
  
  private def eject(accountNo:Int, state:STATE):STATE = {
    if (state.alarms.contains(accountNo)){
      val newalarms = state.alarms.filter(no => no != accountNo)
      STATE(state.accounts, state.blocked, newalarms)
    }else{state}
  }    
}

sealed trait BackendMsg
case class AccountNo(account:Int) extends BackendMsg
case class AccountName(name:String) extends BackendMsg
case object AccountAll extends BackendMsg
case class IsPinValid(accNo:Int, pin:String) extends BackendMsg
case class NewAccount(balance:Int, pin:String, name:String) extends BackendMsg
case class Balance(accNo:Int, pin:String) extends BackendMsg
case class Transactions(accNo:Int, pin:String) extends BackendMsg
case class Withdraw(from:Int, pin:String, amount:Int) extends BackendMsg
case class Deposit(to:Int, amount:Int) extends BackendMsg
case class Transfer(from:Int, to:Int, pin:String, amount:Int) extends BackendMsg
case class ChangePin(accountNo:Int, oldPin:String, newPin:String) extends BackendMsg
case class Block(accountNo:Int) extends BackendMsg
case class Eject(accountNo:Int) extends BackendMsg  
//case object STOP
  

object backend {
  
  // the actor system
  private val system = ActorSystem("ATMBackend")
  private var pri_actor:ActorRef = _
  
  
  // timeout
  implicit val timeout = Timeout(1 seconds)
  
  //Interface
  def start():ActorRef = {
    assert(system.actorFor("/user/backend").isTerminated, "ATM backend has been started.")
    pri_actor = system.actorOf(Props(new backend()), "backend")
    pri_actor
  }
  
  def start_link():Actor = {
    //assert(pri_actor == null,  "ATM has been started.")
    val actor = new backend()
    pri_actor = actor.self
    actor
  }
  
  def update_actor(actor:ActorRef) {
    pri_actor = actor
  }
  
  def stop() {
    val a = system.actorFor("/user/backend")
    //println(a.isTerminated)    
    system.stop(a)
    //Thread.sleep(1000)
    //println(a.isTerminated)    
    system.shutdown()
  }

  def accountNo(account:Int):Account = {    
    val res = ask(pri_actor, AccountNo(account)).mapTo[Account]
    Await.result(res, 1 second)
  }

  def accountName(name:String):List[Account] = {
    val res = ask(pri_actor, accountName(name)).mapTo[List[Account]]
    Await.result(res, 1 second)
  }
  
  def accountAll():List[(Int, String)] = {
    val res = pri_actor.ask(AccountAll).mapTo[List[(Int, String)]]
    Await.result(res, 1 second)
  }
  
  def pin_valid(accNo:Int, pin:String):Boolean = {
    val res = (ask(pri_actor, (IsPinValid(accNo, pin)))(timeout)).mapTo[Boolean]
    Await.result(res, 1 second)
  }
  
  def change_pin(accountNo:Int, oldPin:String, newPin:String):Any = {
    val res = pri_actor.ask(ChangePin(accountNo, oldPin, newPin), 1000)//.mapTo[Symbol]
    Await.result(res, 1 second)
  }
  
  def balance(accNo:Int, pin:String):Any = {
    val res = pri_actor.ask(Balance(accNo, pin))//.mapTo[Int]
    Await.result(res, 1 second)
  }
  
  def transactions(accNo:Int, pin:String):Any = {//:List[Transaction] = {
    val res = pri_actor.ask(Transactions(accNo, pin))//.mapTo[List[Transaction]]
    Await.result(res, 1 second)
  }

  def withdraw(from:Int, pin:String, amount:Int):Any = {//:Symbol = {
    val res = pri_actor.ask(Withdraw(from, pin, amount))//.mapTo[Symbol]
    Await.result(res, 1 second)
  }

  def deposit(to:Int, amount:Int):Symbol = {
    val res = pri_actor.ask(Deposit(to, amount)).mapTo[Symbol]
    Await.result(res, 1 second)
  }
  
  def transfer(from:Int, to:Int, pin:String, amount:Int):Any = {//:Symbol = {
    val res = pri_actor.ask(Transfer(from, to, pin, amount))//.mapTo[Symbol]
    Await.result(res, 1 second)
  }

  def block(accountNo:Int):Symbol = {
    val res = pri_actor.ask(Block(accountNo)).mapTo[Symbol]
    Await.result(res, 1 second)
  }
  
  def eject(accountNo:Int):Symbol = {
    val res = pri_actor.ask(Eject(accountNo)).mapTo[Symbol]
    Await.result(res, 1 second)
  }
}
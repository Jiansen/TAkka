package sample.atm.takka

import java.util.Calendar;

sealed trait TransactionType
case object DepositType extends TransactionType
case object WithdrawType extends TransactionType



case class Transaction(trans_type:TransactionType, date:Calendar, amount:Int)

class Account(var no:Int, var balance:Int, var pin:String, val name:String, var transactions:List[Transaction]) {
  assert(no>0, "Initial Error: account number must be a positive integer.")
  
  override def toString():String = {
     "\n (account number: "+no+
     "\n  balance: "+balance+
     "\n  pin: "+pin+
     "\n  name: "+name+
     "\n  transactions: "+transactions+
     "\n )\n"
  }
}

object db_list {  
  def empty() : List[Account] = {Nil}
  
  // Finds a given account in the database
  def lookup(no:Int, db:List[Account]) : Option[Account] = {
    db.find(a => a.no == no)
  }
  
  // Finds a given account in the database, based on the #account# infomation
  def lookup_all(key:Any, key_name:Symbol, db:List[Account]):List[Account] = {
    db.filter(a => (key, key_name) match {
        case (no:Int, 'number) => no == a.no 
        case (balance:Int, 'balance) => balance == a.balance
        case (pin:String, 'pin) => pin == a.pin
        case (name:String, 'name) => name == a.name
        case (transactions:List[Transactions], 'transactions) => transactions == a.transactions
        case _ => false
    }
    )
  }

  def update(acc:Account, db:List[Account]):List[Account] = db match {
    case Nil => List(acc)
    case current::rest =>
      if (current.no == acc.no) {
        acc::rest
      }else{
        current::(update(acc, rest))
      }
  }
  
  def close(a:List[Account]):Unit = {Unit}
  
  //Converts the database to a list
  def db_to_list(L:List[Account]):List[Account] = {L}

  //Adds a new record to the DB
  def insert(acc:Account, db:List[Account]):List[Account] = db match {
    case Nil => List(acc)
    case current::rest => 
      if (current.no == acc.no) {
        throw new Exception("account "+acc.no+" exists in database.")
      }else{
        current::(insert(acc, rest))
      }
  }
  
  //Returns the size of the database
  def db_size(db:List[Account]):Int = {db.length} 
}

/*
object test extends App{
  val d1 = db_list.empty()
  println(d1)
  val d2 = db_list.insert(new Account(42, 0, "undefined", "Henry", Nil), d1)
  println(d2)
  println(db_list.lookup(42, d2))
}
*/
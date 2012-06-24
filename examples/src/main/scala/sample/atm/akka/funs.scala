package sample.atm.akka

object funs {
  def user(name:String, accounts:List[Account]):List[Account] = {
    accounts.filter(a => a.name == name)
  }
  
  def user1(name:String, accounts:List[Account]):List[Account] = {
    accounts.foldLeft[List[Account]](Nil)((acc, a) => if (a.name == name) {a::acc} else acc)
  }

  def user_accounts(name:String, accounts:List[Account]):List[Int] = {
    accounts.foldLeft[List[Int]](Nil)((acc, a) => if (a.name == name) {a.no::acc} else acc)
  }
  
  def name_change(name:String, nameNew:String, accounts:List[Account]):List[Account] = {    
    accounts.map(a =>
      if (a.name == name) new Account(a.no, a.balance, a.pin, nameNew, a.transactions)
      else a
      )
  }   
}
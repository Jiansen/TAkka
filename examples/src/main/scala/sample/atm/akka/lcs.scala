package sample.atm.akka

object lcs {
  def user(name:String, accounts:List[Account]):List[Account] = {
    accounts.filter(a => a.name == name)
  }
  
  def user_accounts(name:String, accounts:List[Account]):List[Int] = {
    for (a<-accounts if a.name == name) yield a.no
  }

  def name_change(name:String, nameNew:String, accounts:List[Account]):List[Account] = {
    for (a<-accounts) yield {
      if (a.name == name) {
        new Account(a.no, a.balance, a.pin, nameNew, a.transactions)
      }else {a}
    }
  }
  
  def disjunction(accounts1:List[Account], accounts2:List[Account]):List[Account] = {
    val intersection = for (a<- accounts2 if accounts1.contains(a)) yield a
    for (a <- accounts1 ++ accounts2 if !intersection.contains(a)) yield a
  }
}
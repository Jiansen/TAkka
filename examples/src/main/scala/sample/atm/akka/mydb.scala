package sample.atm.akka

import scala.collection.mutable.{HashMap}

object mydb {
  private object table extends HashMap[Symbol, List[Any]]
  
  //def all():List[Any] = { }

  def insert(tab:Symbol, element:Any) {
    table(tab).+:(element)
  }
  
  def delete(tab:Symbol):Boolean = {
    table.remove(tab)
    true
  }
  
  def deleteWith(tab:Symbol, filter:Any=>Boolean):Unit = {
    val backup = table.clone()
    try{
      val tmp = table(tab).filterNot(filter)     
      table.clear()
      table ++ tmp
    }catch{
      case e:Exception => 
        table.clear()
        table ++ backup.clone()
    }
  }
  
  def matchWith(tab:Symbol, filter:Any=>Boolean):List[Any] = {
    try{
      table(tab).filter(filter)     
    }catch{
      case e:Exception => Nil
    }
    Nil
  }
  
  def updateWith(tab:Symbol, filter:Any=>Any):Unit = {
    val backup = table.clone()
    try{
      //table(tab).filter(filter)
      val tmp = for (a <- table(tab)) yield {filter (a) }
      table.clear()
      table ++ tmp
    }catch{
      case e:Exception =>
        table.clear()
        table ++ backup.clone()
    }
  }
  
}
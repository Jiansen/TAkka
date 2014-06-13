import join._

object CounterJoin extends Join {
  var count: Long = 0

  object GetAndReset extends SynName[Unit, Long]
  object AddCount extends AsyName[Long]
  
  join{
        case GetAndReset(_) =>
          val current = count
          count = 0
          GetAndReset reply current
        case AddCount(extraCount) =>
          count=count+extraCount
  }
}
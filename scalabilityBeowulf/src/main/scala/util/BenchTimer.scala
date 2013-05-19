package util

class BenchTimer{
  private var startTime:Long = _
  private var finishTime:Long = _
  
  def start = {
    startTime = System.currentTimeMillis()
  }
  def finish = {
    finishTime = System.currentTimeMillis()
  }
  def report = {
    println("\n elapse: "+(finishTime - startTime)+" milliseconds.\n");
  }
}
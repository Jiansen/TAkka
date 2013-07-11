package util

class BenchCounter{
    private var n:Int = 0;
    def set(n:Int) = synchronized {
      this.n = n
    }
    def increment = synchronized {
      n = n+1
    }
    def decrement = synchronized {
      n = n-1
    }
    def isZero:Boolean = synchronized {
      return (n == 0)
    }
    def get:Int = {return n;}
}
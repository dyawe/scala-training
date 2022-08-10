import scala.annotation.tailrec

object Recursion {

  //QUESTION TWO
  def addRec(start: Int, end: Int): Int = {
    @tailrec
    def add(start: Int, end: Int, acc: Int): Int ={
      if (start == end) acc //> if we want to add last number and == if we do not want to
      else (add(start + 1, end, acc + start))
    }
    add(start, end, 0)
  }

  //QUESTION THREE
  def concatString(n: Int, text: String): String = {
    @tailrec
    def concatStringWithAcc(n: Int, acc: String): String = {
      if (n == 1) acc
      else concatStringWithAcc(n - 1, acc + text)
    }
    concatStringWithAcc(n, text)
  }

  //QUESTION FOUR
  def strlen(str: String): Int ={
    @tailrec
    def count(str: String, acc: Int): Int = {
      if (str.tail == "") acc
      else count(str.tail, 1+acc)
    }
    count(str,0)
  }

  //QUESTION FIVE
  def fibSeries(n: Int): Int ={
    @tailrec
    def fib(n: Int, prev: Int, next: Int): Int ={
      if (n <= 0) 0
      else if (n == 1) prev
      else fib(n-1, next, prev + next)
    }
    fib(n: Int, 0,1)
  }

  //QUESTION SIX
  def isPrime(n: Int): Boolean = {
    @tailrec
    def primeRecursive(iterator: Int, n: Int): Boolean = {
      if (iterator == n) {
        true
      } else if (n % iterator == 0) {
        false
      } else {
        primeRecursive(iterator + 1, n)
      }
    }
    primeRecursive(2, n)
  }


  def main(args: Array[String]): Unit =
  {
    println(concatString(3, "yawe"))

    val str1 = ""
    val str2 = "friggatriskaidekaphobia"
    println(strlen(str1))
    println(strlen(str2))

    println(fibSeries(8))

    println(addRec(1,3))

    println(isPrime(41))

  }

}

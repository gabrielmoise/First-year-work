object Fib
{

  var F = Array (1,1,1,0)

  def Fib (n : Int) : Int ={
    var A = Array (1,0,0,1)
    var X = Array (1,1,1,0)
    var r = n
    while (r != 0)
    {
      if (r % 2 == 0) {X = mult (X,X) ; r = r / 2}
      else {A = mult (A,X) ; X = mult (X,X) ; r = r / 2}
    }
    return A(2)
  }

  def mult (B : Array[Int], C : Array[Int]) : Array[Int] = {
    var R = new Array [Int] (4)
    R(0) = B(0) * C(0) + B(1) * C(2)
    R(1) = B(0) * C(1) + B(1) * C(3)
    R(2) = B(2) * C(0) + B(3) * C(2)
    R(3) = B(2) * C(1) + B(3) * C(3)
    return R
  }

  def main (args : Array[String]) = {
    var A = Array (1,0,0,1)
    var X = Array (1,1,1,0)
    var n = scala.io.StdIn.readInt
    var r = n
    // Invariant F^n = A * X^r
    while (r != 0)
    {
      if (r % 2 == 0) {X = mult (X,X) ; r = r / 2}
      else {A = mult (A,X) ; X = mult (X,X) ; r = r / 2}
    }
    // r = 0 => F^n = A
    println (Fib(n))
  }
}

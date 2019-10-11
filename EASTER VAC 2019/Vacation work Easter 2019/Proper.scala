object Question4 {
  def reduce (m : Int, n : Int) : (Int,Int) = {
    var a = m
    var b = n
    // m/n can be simplified into a fraction which is in "its lowest terms" if we simplify it by gcd(m,n), which we will calculate
    // Invariant I : gcd(m,n) = gcd(a,b)
    while (b != 0)
    {
      var r = a%b
      a = b
      b = r
      // Here, we use the fact that gcd(a,b) = gcd(b,a%b)
    }
    // b = 0 => gcd(m,n) = a
    return (m/a,n/a)
  }

  def fromRat(m1 : Int, n1 : Int) : Array[Int] = {
    var m = m1 ; var n = n1
    var e = new Array[Int](1000)
    var k = 0
    // Invariant I : m1/n1 = m/n + sum for i from 0 until k of 1/e(i)
    while (m != 0)
    {
      var q = n/m
      if (n % m != 0) q += 1
      // q = ceiling(n/m)
      e(k) = q
      k += 1
      m = m*q - n
      n = n*q
    }
    // m = 0 => m1/n1 = sum of Egyptian fractions
    e
  }

  def toRat (e : Array[Int]) : (Int,Int) = {
    var N = e.size
    var m = 0
    var n = 1
    var i = 0
    // Invariant I : m/n = sum from 0 until i of 1/e(j)
    while (i < N)
    {
      m = m*e(i) + n
      n = n*e(i)
      i += 1
    }
    reduce(m,n)
  }

  def printRat (m : Int, n : Int) : String = m.toString+"/"+n.toString

  def printEgypt (e : Array[Int]) : String = {
      var N = e.size
      while (e(N-1) == 0) N -= 1 // when the size of the array is bigger than the number of elements in it
      var str = ""
      for (i <- 0 until N-1) str = str + "1/" + e(i).toString + "+"
      str = str + "1/" + e(N-1)
      str
    }

  def printEq (m : Int, n : Int) : String = {
    printRat(m,n) + " = " + printEgypt(fromRat(m,n))
  }

  def main (args : Array[String]) = {
    println(reduce(14,35)._1+"/"+reduce(14,35)._2)
    var a = new Array[Int](1000)
    a = fromRat(2,5)
    for (i <- 0 until 2) print(a(i)+" ")
    println()
    var e = Array(2,5,70)
    println(toRat(e)._1+"/"+toRat(e)._2)
    println(printRat(5,7))
    println(printEgypt(e))
    println(printEq(12,49))
  }
}

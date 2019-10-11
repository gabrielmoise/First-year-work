object Composite {

  // (a)
  def record (f : (Int => Int), emit : (Int => Unit)) = {
    var N = 100
    var nrmax = 0
    var printed = 0
    var i = 1
    while (printed <= N)
    {
      var x = f(i)
      if (x > nrmax)
        {
          emit(i)
          printed += 1
          nrmax = x
        }
      i += 1
    }
  }

  // (b)
  def divisors(n : Int) : Int = {
    var div = 0
    var i = 1
    // Invariant I : div is the number of divisors of n in [1..i)
    while (i <= n/2)
    {
      if (n % i == 0) div += 1
      i += 1
    }
    // for every n/2 < i < n, i cannot divide n
    div += 1 // n divides n
    return div
  }

  // (c) O(n^2)

  // (d)
  /*
    The number of divisors is (i1+1)(i2+1)*...*(ik+1) because every divisor
    of n is formed of powers of p1,p2,...,pk at powers ranging from
    0 to ii. So, by the product rule, that is the formula(they cannot repeat
    since p1,p2,...,pk are prime.
  */

  // (e)
  def divisorsFast (n : Int) : Int = {
    var aux = n
    var div = 1
    var i = 2
    // div = (i1+1)*(i2+1)*...*(it+1) where i1,i2,...,it are the prime divisors of
    // n up to i
    while (aux != 1)
    {
      var nrdiv = 0
      while (aux % i == 0) {nrdiv += 1 ; aux = aux / i}
      div = div * (nrdiv+1)
      if (i == 2) i += 1
          else i += 2 // skipping the even numbers
    }
    return div
  }

  def main (args : Array [String]) = {
    /*
    println(divisors(1))
    println(divisors(12))
    println(divisors(17))
    println(divisorsFast(1))
    println(divisorsFast(12))
    println(divisorsFast(17))
    */
    record(divisors, print)
  }
}

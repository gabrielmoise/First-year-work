object Question1
{
  // (a)
  def digsToInt (xs : Array [Int], bs : Array[Int]) : Int = {
    var N = xs.size
    var i = N-1
    var res = xs(N-1)
    // Invariant I : res = sum from j=i to N-1 of (xs(j) * product [i..j))
    while (i > 0)
    {
      res = res * bs(i-1)
      res = res + xs(i-1)
      i -= 1
    }
    // i = 0 => res = sum ...
    // and we only needed N multiplications
    res
  }
  def intToDigs (n : Int, bs : Array[Int]) : Array[Int] = {
    var N = bs.size
    var xs = new Array[Int](N)
    var res = n
    var i = 0
    // Invariant : So, far we have calculated xs[0..i)
    while (i<N)
    {
      xs(i) = res % bs(i)
      res = res / bs(i)
      i += 1
    }
    xs
  }
  def main (args: Array[String]) =
    {
      println(digsToInt(Array(9,8,7,5), Array(10,10,10,10)))
      var a = new Array[Int](4)
      a = intToDigs(5789,Array(10,10,10,10))
      println(a(0)+", "+a(1)+", "+a(2)+", "+a(3))
    }
}

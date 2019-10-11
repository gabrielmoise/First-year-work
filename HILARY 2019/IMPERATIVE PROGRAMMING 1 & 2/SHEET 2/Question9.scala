object Question9
{
  def log3 (x: Double) : Int =
    {
      var y = 0 // log3 of x with x>=1 is guaranteed to be at least 0, so the floor is also at least 0, so y is a non-negative integer
      var p = 1.00
      // Invariant: p = 3^y
      // variant x-p/3
      while (p<=x)
      {
        p = p * 3
        // p = 3^(y+1)
        y += 1
        // I
      }
      // We got out of the loop, so p>x, and because at the previous step we were in the loop, then
      // p/3 is the biggest power of 3 which is less than or equal to x
      // Because p = 3^y from the invariant, p/3=3^(y-1), so (y-1) is the floor of log3(x), so we return (y-1)
      (y-1)
    }
  def main (args : Array[String]) =
    {
      assert(log3 (1) == 0)
      assert(log3 (2) == 0)
      assert(log3 (3) == 1)
      assert(log3 (26) == 2)
      assert(log3 (27) == 3)
      assert(log3 (1000) == 6)
    }
}

object Simpson
{
  def kahan_sum(n: Int, f: (Int => Double)) = {
    var sum = f(0)
    var c = 0.0

    for(i <- 1 to n){
      var y = f(i) - c
      var t = sum + y
      c = (t - sum) - y
      sum = t
    }

    sum
  }

  def simpson_value(i: Int, n: Int, l: Double, r: Double, f: (Double => Double)) =
    (if(i == 0 || i == n) 1 else 2 + 2 * (i%2)) * f(((r - l) * i) / n + l)

  def simpson_kahan(n: Int, l: Double, r: Double, f: (Double => Double)) =
    kahan_sum(n, { simpson_value(_, n, l, r, f) } ) * (2.0 / 3.0) * (1.0 / n)

  def simpson (n : Int) : Double =
    {
      var sum = 0.00
      sum = sum + Math.pow(0,1.5)
      for (i<-1 until n)
      {
        if (i%2==0) sum = sum + 2*Math.pow(2.0*i/n,1.5)
            else sum = sum + 4*Math.pow(2.0*i/n,1.5)
      }
      sum = sum + Math.pow(2,1.5)
      sum = (sum*2.0)/(3.0*n)
      sum
    }
  def main (args: Array[String]) =
    {
      val trials= scala.io.StdIn.readInt
      var N = 1
      val integral = 8.0*Math.pow(2,0.5)/5.0
      for (j<-1 to trials)
      {
        N = N * 2
        val approx = simpson_kahan(N,0,2,{x => Math.pow(x,1.5)})
        val error = approx - integral
        println("Estimate using "+N+" strips is "+approx+" and the error is "+error)
      }
    }
}

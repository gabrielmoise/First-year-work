object Problem
{
  val lambda = 3.0 : Double

  def iter (xn : Double) = xn - (Math.exp(lambda*xn-1.0) - xn)/(lambda*Math.exp(lambda*xn-1.0) - 1.0)

  def main (args: Array[String]) =
    {
      var xn = 1.0 : Double
      val tol = 1e-10
      var count = 0 : Long
      while ((Math.abs(xn-iter(xn)) > tol) && (count<100))
      {
        xn = iter(xn)
        count = count + 1
        println("x("+count+")= "+xn)
      }

    }
}

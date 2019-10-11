object Problem
{
  val tol = 1e-10
  val N = 100

  def funct (xn : Double, yn : Double) : (Double, Double) =
    {
      var x = 1.0 / ((2.0-xn)*(2.0-yn)) - xn
      var y = 2.0 / ((3.0 - 2.0*xn)*(3.0 - yn)) - yn
      return (x,y)
    }

  def system (xn : Double, yn : Double) : (Double, Double) =
    {
      var A = 1.0 / ((2.0-xn)*(2.0-xn)*(2.0-yn)) - 1.0
      var B = 1.0 / ((2.0-xn)*(2.0-yn)*(2.0-yn))
      var C = 4.0 / ((3.0-2.0*xn)*(3.0-2.0*xn)*(3.0-yn))
      var D = 2.0 / ((3.0-2.0*xn)*(3.0-yn)*(3.0-yn)) - 1.0
      var E = xn - 1.0 / ((2.0-xn)*(2.0-yn))
      var F = yn - 2.0 / ((3.0-2.0*xn)*(3.0-yn))
      var x = (D*E - B*F) / (A*D - B*C)
      var y = (A*F - C*E) / (A*D - B*C)
      return (x,y)
    }

  def size (xn : Double, yn : Double) : Double =
    {
      var (x,y) = funct(xn,yn)
      var aux = x*x + y*y
      return Math.pow(aux,0.5)
    }

  def Newton (arg1 : Double, arg2 : Double) : (Double, Double,Int) =
    {
      var n = 0
      var xn = arg1
      var yn = arg2
      while ((n != N) && (size(xn,yn) > tol))
      {
        var (x,y) = system(xn,yn)
        xn = xn + x
        yn = yn + y
        n = n + 1
      }
      return (xn,yn,n)
    }

  def main (args: Array[String]) =
    {
      var res = Newton(0.0, 0.0)
      println(res)
    }
}

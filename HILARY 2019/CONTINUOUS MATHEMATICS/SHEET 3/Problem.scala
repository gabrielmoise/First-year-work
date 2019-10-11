object Problem
{
  def function (x : Double, y : Double) : Double = 2.25 * x - 0.5 * y
  def main (args : Array[String]) =
    {
      val x = new Array [Double] (100)
      x(0) = 1.0/3.0
      x(1) = 1.0/12.0 
      println("x(1) = "+x(0))
      println("x(2) = "+x(1))
      val k = 100
      for (i <- 2 until k)
      {
        x(i) = function (x(i-1),x(i-2))
        println("x("+(i+1)+") = "+x(i))
      }
    }
}

object Problem
{
  val root = Math.log(2.0) : Double
  val L = 2.0 : Double
  val tol = 1e-10

  def iter(xn : Double) : Double = xn + L*(Math.exp(-xn)) - 1

  def main (args: Array [String]) =
    {
      var x = 100.0 : Double
      var count = 0 : Long
      println("x("+count+")= "+x)
      while (Math.abs(root-x) > tol*root)
      {
        x = iter (x)
        count = count + 1
        println("x("+count+")= "+x)
      }
      println("Root is "+root)
      println("We needed "+count+" steps")
    }
}

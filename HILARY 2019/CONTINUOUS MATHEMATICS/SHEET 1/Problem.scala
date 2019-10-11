object Problem
{
  def main (args: Array[String]) =
  {
      var ok = 0
      var k = 1
      val boundodd : Double = 3.08616127
      val boundeven : Double = 2.35040239
      val precision : Double = 0.000000000000001
      var error : Double = 0.0
      while (ok==0)
      {
        if (k%2==0)
        {
          error = boundeven
          for (i<-1 to (k+1)) error = error / i
          if (error <= precision) ok = 1
        }
        else
        {
          error = boundodd
          for (i<-1 to (k+1)) error = error / i
          if (error <= precision) ok = 1
        }
        k = k + 1
      }
      k = k - 1
      println("The order of the Taylor polynomial is "+k+" and the error is "+error)
  }
}

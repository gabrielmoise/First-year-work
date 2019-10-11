object Question10
{
  /** Calculating the value of the poynomial with coefficients in the array a, at x */
  def eval(a: Array[Double], x: Double) : Double =
    {
      val n = a.size
      var i = 0
      var powx = 1.00
      var result = 0.00
      // Invariant I: result = the sum of the first i terms of the polynomial && powx = x^i && 0<=i<=n
      // variant (n-i)
      while (i<n)
      {
        // I && 0<=i<n
        result += powx * a(i)
        // result = the sum of the first (i+1) terms of the polynomial && powx = x^i
        i += 1
        // result = the sum of the first i terms of the polynomial && powx = x^(i-1) && 0<=i<=n
        powx = powx * x
        // I
      }
      // i=n so result = the sum of the first n terms of the polynomial
      result
    }
    // Notice that we do 2*n multiplications with this method
    def evalHarder(a: Array[Double], x: Double) : Double =
      {
        // Here we do only (n-1) multiplications to determine the value of the polynomial in x
        val n = a.size
        var i = 0
        var result = a(n-1) // = a(n-1) * x^0
        // Invariant I: result = sum(a(n-1-j)*x^(i-j)) with j from 0 to i && 0<=i<=n
        // variant (n-1-i)
        while (i<n-1)
        {
          // I && 0<=i<n
          result = result * x
          // result = sum (a(n-1-j)*x^(i-j+1)) with j from 0 to i, equivalent to
          // result = sum (a(n-k)*x^(i-k+2)) with k from 1 to (i+1), by setting k=j+1
          i += 1
          // result = sum (a(n-k)*x^(i-k+1)) with k from 1 to i, equivalent to
          // result = sum (a(n-1-j)*x^(i-j)) with j from 0 to (i-1), by setting j=k-1
          result = result + a(n-1-i)//*x^0
          // I
        }
        // i=n-1, from I we have result = sum(a(n-1-j)*x^(n-1-j)) with j from 0 to (n-1) and that's the result we needed
        // We made (n-1) multiplications, one for each iteration of the loop
        result
      }
}

object Question4 {
  var m = Array(0,1,2,3,4,5,6,7,8,9,10)

  // swap the values of m(i) and m(j)
  // cost = 4
  def swap (i : Int, j : Int) : Unit = {
      var aux = m(i)
      m(i) = m(j)
      m(j) = aux
  }

  // exchange the values from m[i..i+n) with m[j..j+n)
  // cost = 4*n
  def blockswap (i : Int, j : Int, n : Int) : Unit = {
      for (q <- 0 until n) swap (i+q,j+q)
  }

  // reverse the order of the values in m[i..i+n)
  // cost = 2*n
  def reverse (i : Int, in : Int) : Unit = {
      var n = in - i
      for (q <- 0 to (n-1)/2) swap (i+q,i+n-1-q)
  }

  // rotate(i,n,k) shifts m[i..(i+n)) by k positions to the right
  // cost = 4*n
  def rotateRev (i : Int, n : Int, k : Int) : Unit = {
    reverse(i,i+n-k) //cost 2*(n-k)
    reverse(i+n-k,i+n) //cost 2*k
    reverse(i,i+n) // cost 2*n
  }

  // cost = O(k*n) because of the recursion
  def rotateBlRec (i : Int, n : Int, k : Int) : Unit = {
    if (n-k == k) blockswap(i,i+n-k,k)
    else if (n-k > k) {blockswap(i,i+n-k,k) ; rotateBlRec(i+k,n-k,k)}
    else {blockswap(i,i+n-k,n-k) ; rotateBlRec(i+n-k,k,n-k)}
  }

  // cost = O(k*n) worst case
  def rotateBl (i1 : Int, n1 : Int, k1 : Int) : Unit = {
    var i = i1 ; var n = n1 ; var k = k1
    while (n-k != k)
    {
      if (n-k > k) {blockswap(i,i+n-k,k) ; i = i+k ; n = n-k}
      else {blockswap(i,i+n-k,n-k) ; i = i+n-k ; var aux = n ; n = k ; k = aux-k}
    }
    blockswap(i,i+n-k,k)
  }

  // cost 2*k*n
  def rotateRep (i : Int, n : Int, k : Int) : Unit = {
    for (q <- 1 to k)
    {
      var t = m(i+n-1) // cost = 1
      var j = i+n-1
      while (j > i) {m(j) = m(j-1) ; j -= 1} // cost = 2*n-2
      m(i) = t // cost = 1
    }
  }
  def main (args: Array[String]) = {


    rotateRep(1,5,2)
    for (i <- 0 until 11) print(m(i)+" ")
    println()
    /*
    rotateRev(1,5,2)
    for (i <- 0 until 11) print(m(i)+" ")
    println()
    swap(0,10) ; swap(5,5)
    for (i <- 0 until 11) print(m(i)+" ")
    println()
    blockswap(1,8,3)
    for (i <- 0 until 11) print(m(i)+" ")
    println()
    reverse (0,11)
    for (i <- 0 until 11) print(m(i)+" ")
    println()
    */

  }

}

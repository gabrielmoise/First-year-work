object Partition {



  def pivot (a : Array[Int], l : Int, r : Int) = a(l)

  def partition(a : Array[Int], l: Int, r: Int) : (Int,Int,Array[Int]) =
    {
      var p = pivot(a,l,r)
      // Invariant I: a[l..i) < pivot && a[i..j) = pivot && a[k..r) > pivot && l <= i < j  <= k <= r
      //            && a[0..l) = a_0[0..l) && a[r..N) = a_0[r..N) && a[l..r) is a permutation of a_0[l..r)
      var i = l ; var j = l+1 ; var k = r
      while (j < k)
      {
        if (a(j) == p) j += 1
          else if (a(j) < p) {var t = a(i); a(i) = a(j); a(j) = t; i += 1 ; j += 1}
          else {var t = a(j); a(j) = a(k-1); a(k-1) = t; k -= 1}
      }
      (i,j,a)
    }

  def find (a : Array[Int], i : Int, l : Int, r : Int) : Int = {
    var (m,n,v) = partition(a,l,r)
    if (i < m) return find(v,i,l,m)
    else if (i < n) return v(m)
    else return find(v,i,n,r)
  }

  // returns the ith smallest element of a[0..N)
  def select (a : Array[Int], i : Int, N : Int) : Int = {
    return find(a,i,0,N)
  }

  def findNonRec (a : Array[Int], i : Int, l : Int, r : Int) : Int = {
    var (m,n,v) = partition(a,l,r)
    var left = l
    var right = r
    while ((m > i) || (n <= i))
    {
      if (i < m) right = m
            else left = n
      var triple = (0,0,Array(0))
      triple = partition(a,left,right)
      m = triple._1 ; n = triple._2 ; v = triple._3
    }
    return v(m)
  }

  def selectNonRec (a: Array[Int], i : Int, N : Int) : Int = {
    return findNonRec(a,i,0,N)
  }

  def main (args: Array[String]) = {
    var v = Array(1,2,1,3,4,0,0,3,1)
    var v1 = new Array[Int](100)
    v1 = partition(v,0,9)._3
    for (i <- 0 until v.size) print(v1(i)+" ")
    println()
    v = Array(1,2,1,3,4,0,0,3,1)
    var v2 = new Array[Int](100)
    v2 = partition(v,4,8)._3
    for (i <- 0 until v.size) print(v2(i)+" ")
    println()
    println(find(v,5,0,9))
    println(find(v,5,5,5))
    println(find(v,5,1,7))
    for (i <- 0 until 9) print(select(v,i,9)+" ")
    println()
    v = Array(1,2,1,3,4,0,0,3,1)
    println(findNonRec(v,5,0,9))
    println(findNonRec(v,5,5,5))
    println(findNonRec(v,5,1,7))
    for (i <- 0 until 9) print(selectNonRec(v,i,9)+" ")
    println()
  }
}

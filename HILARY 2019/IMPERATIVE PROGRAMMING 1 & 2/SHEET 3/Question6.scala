object Question6
{
/** Partition the segment a[l..r)
* return k s.t. a[l..k) < a[k..r) and l <= k < r */
def partition(l: Int, r: Int, a:Array[Int]) : Int =
  {
    val x = a(l)
    // pivot
    // Invariant a[l+1..i) < x = a(l) <= a[j..r) && l < i <= j <= r
    //            && a[0..l) = a_0[0..l) && a[r..N) = a_0[r..N)
    //            && a[l..r) is a permutation of a_0[l..r)
    var i = l+1; var j = r
    while(i < j)
    {
      if(a(i) < x) i += 1
        else {val t = a(i); a(i) = a(j-1); a(j-1) = t; j -= 1 }
    }
    // swap pivot into position
    a(l) = a(i-1); a(i-1) = x
    i-1 // position of the pivot
  }
/**
An example to ilustrate the fact that an element that is bigger than the pivot can be moved twice before finding
its place is : 4 3 5 6, with the pivot a(0) = 4, i = 1, j = 4. First, we skip a(1) = 3 (i=2), then we see that 5>=4, so we swap 5 with 6 to get
4 3 6 5 (at this point i=2, j=3). Then, we see that 6 >= 4, so we swap again 6 with 5 to get 4 3 5 6 (i=j=2, and we stop, the position of the pivot
is 1). So, for 6 we needed 2 comparisons to find its place and we swapped it twice, although we didn't need to swap it at all.
*/
  def partition2 (l: Int, r: Int, a:Array[Int]) : Int =
    {
      val x = a(l)
      // pivot
      // Invariant a[l+1..i) < x = a(l) <= a[j..r) && l < i <= j <= r
      //            && a[0..l) = a_0[0..l) && a[r..N) = a_0[r..N)
      //            && a[l..r) is a permutation of a_0[l..r) (same invariant as before)
      var i = l+1; var j = r
      while(i < j)
      {
        if(a(i) < x) i += 1
          else
            {
              // Invariant J : x <= a[j..j_0), where j_0 is the value of j before getting in the while-loop
              // Also i+1<j so that we don't get out of the segment we want to partition
              while ((a(j-1) >= x) && (i+1<j)) j -= 1
              // This way, we only swap a(i) with a(j-1) if a(j-1) is smaller than x, so that we don't have to swap it again next time
              val t = a(i); a(i) = a(j-1); a(j-1) = t; j -= 1
            }
      }
      // swap pivot into position
      a(l) = a(i-1); a(i-1) = x
      i-1 // position of the pivot
    }
  def main (args: Array[String]) =
    {
      val a1 = Array(1,2,3,4,5,6)
      val b1 = Array(1,2,3,4,5,6)
      assert(partition(0,6,a1) == partition2(0,6,b1))
      val a2 = Array(6,5,4,3,2,1)
      val b2 = Array(6,5,4,3,2,1)
      assert(partition(0,6,a2) == partition2(0,6,b2))
      val a3 = Array(4,1,2,1,4,5,6,1)
      val b3 = Array(4,1,2,1,4,5,6,1)
      assert(partition(0,8,a3) == partition2(0,8,b3))
      val a4 = Array(1,1,1,1)
      val b4 = Array(1,1,1,1)
      assert(partition(0,4,a4) == partition2(0,4,b4))
      // Used two arrays for the assertions as each call to any partition function changes the array.
    }
}

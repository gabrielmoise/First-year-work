object Question8
{
  val a = Array (2,6,2,1,5,2,1,7,4,0)
  def partition(l: Int, r: Int) : Int =
  {
    val x = a(l)
    var i = l+1; var j = r
    while(i < j)
    {
      if(a(i) < x) i += 1
        else {val t = a(i); a(i) = a(j-1); a(j-1) = t; j -= 1 }
    }
    a(l) = a(i-1); a(i-1) = x
    i-1
  }
def QSort(l: Int, r: Int) : Unit =
  {
    if(r-l > 1)
      {
        val k = partition(l,r)
        QSort(l,k); QSort(k+1,r)
      }
  }
/**
(a) If the array we want to sort has all the elements equal, we see that the partition function would
return (in O(N) operations) the pivot always to be the l, so the two QSort calls will be
QSort(l,l) which will take O(1) and QSort(l+1,r). Therefore, we will need O(N) calls to QSort, so
the time complexity will be O(N^2).
(b) If the array has a lot of identical entries in the array the partitioning may return an array that is partitioned
with a pivot that is very close to one end, and consequently very far from the other one and thus the complexity is
going to be closer to O(N^2) than to O(N*log2(N)). This depends on how big the identical entries are compared to the
others (it will be optimal if they are at the middle of the sorted array).
(c) In the partition function, if there are multiple identical entries, then there would be more else clauses executed
in the if statements because the condition is with "<". This means that we will have more instructions to execute since
the else clause has 4, whereas the if clause has just 1. So, on average, we do more work by swapping unnecessary elements.
Thus, if we replace the "<" in the if statement with "<=", in the case where there are more identical entries, we will
need less instructions to execute.
*/
// (d)
def partition2(l: Int, r: Int) : (Int,Int) =
  {
    val pivot = a(l)
    // pivot
    // Invariant I: a[l..i) < pivot && a[i..j) = pivot && a[k..r) > pivot && l <= i < j  <= k <= r
    //            && a[0..l) = a_0[0..l) && a[r..N) = a_0[r..N) && a[l..r) is a permutation of a_0[l..r)
    var i = l ; var j = l+1 ; var k = r
    while (j < k)
    {
      if(a(j) == pivot) j += 1
        else if (a(j) < pivot) {val t = a(i); a(i) = a(j); a(j) = t; i += 1 ; j += 1 }
        else {val t = a(j); a(j) = a(k-1); a(k-1) = t; k -= 1 }
    }
    (i,j)
  }
// (e)
  def QSort2(l: Int, r: Int) : Unit =
    {
      if(r-l > 1)
        {
          val (i,j) = partition2(l,r)
          QSort(l,i) ; QSort(j,r) // There is no need to sort the elements a[i..j) since they are all equal from partition2(l,r).
        }
    }
  def main (args: Array[String]) =
    {
      println(partition2(0,10))
      for (i<-0 until 10) print(a(i)+" ")
      QSort2(0,10)
      println("")
      for (i<-0 until 10) print(a(i)+" ")
    }
}

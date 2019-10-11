object Question7
{
  val a = ...
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
  /**
  (a) We use a while-loop instead of the second call to QSort because after we are
  done with the first QSort call in the initial algorithm, we only need to modify the value of l to (k+1) for the second QSort.
  So, we keep l in a variable aux, which can be modified
  (b) In the worst case-scenario, the stack might be O(N), as we might QSort over the bigger partition
  of the sub-array we are currently sorting(the left sub-array), so in the case when the calls are for
  sorting an array of size N, then (N-1), then (N-2) ... and finally 1, the call stack reaches depth N.
  (c) In the QSort2 version of the algorithm, we choose the smaller partition and we recurse on it(the size of the smaller partition is maximum floor(N/2)), so that in the worst
  case scenario we have a ceiling(log2(N)) depth of the stack call (worst-case scenario : if at every step the sub-array is partitioned
  exactly in half) : 2^k, 2^(k-1), ... , 2^1, 1 -> k+1 stack calls, so ceiling(log2(N)).(if N is not a power of 2, we need at most
  this).
  */
  def QSort (l : Int, r : Int) : Unit =
    {
      var aux = l
      // Invariant I : a[l..aux) is sorted and all the elements from a[aux..r) are greater than a[l..aux]
      while (r-aux > 1)
      {
        var k = partition(aux,r)
        QSort (aux,k) // We can have QSort(l,k) as mentioned in the task, but it is very inefficient
        aux = k + 1
      }
    }
  def QSort2 (l : Int, r : Int) : Unit =
    {
      var left = l ; var right = r
      // Invariant I : a[l..left) is sorted and a[right..r) is sorted and a[l..left) < a[left..right) < a [right..r)
      while (right-left > 1)
      {
        var k = partition(left,right)
        if (k-left<right-1-k) { QSort2(l,k) ; left = k + 1 }
            else              { QSort2(k+1,r) ; right = k }
      }
    }
}

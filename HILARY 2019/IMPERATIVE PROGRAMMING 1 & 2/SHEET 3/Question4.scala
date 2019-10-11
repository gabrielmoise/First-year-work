object Question4
{
  def insert (a : Array [Int]) : Array[Int] =
    {
      val N = a.size
      var pos = 1
      // Invariant I : a[0..pos) sorted increasingly && 1 <= pos <= N
      // Variant (N-pos)
      while (pos < N)
      {
        // I && 1 <= pos < N
        var el = a(pos)
        var k = binary (a,pos)
        // We need to move the elements a(k),a(k+1)...a(pos-1) to the right with a position
        var i = pos
        // Invariant J : a[i..pos) are shifted to the right && there is an empty cell in a(i) && k <= i <= pos
        // Variant (i-k)
        while (i > k) { a(i) = a(i-1); i -= 1 }
        a(k) = 0
        // J && i==k => a(k) has no element in it, so we put there a(pos)
        a(k) = el
        // a [0..(pos+1)) sorted increasingly
        pos += 1
        // I
      }
      // pos = N && I => a[0..N) is sorted increasingly, so we return a
      a
    }
  // Finding the place in a[0..pos) where we should put a(pos), binarily.
  def binary (a : Array[Int], position : Int) : Int =
    {
        var X = a(position)
        // First, we'll treat the limit cases :
        if (a(0) >= X) return 0
        if (X >= a(position-1)) return position
        var left = 0 ; var right = position-1
        // Invariant I : a(left) < X <= a(right)
        while (left + 1 < right)
        {
          var mid = (left+right) / 2 // left < mid < right
          if (X > a(mid)) left = mid
              else right = mid
          // I
        }
        // I holds and left + 1 = right => a(left) < X <= a(left + 1), so we have to put X on the (left+1)th position
        (left + 1)
    }
  /**
    We have N calls of the function binary. Each call does around O(log2(position)) operations as we make 2 comparisons initially
  and then in the while loop where the difference between right and left halves after each step (each step is one comparison)
  and we start with the difference being position-1. As position takes each value from 1 to N, we have
  log2(1) + log2(2) + ... + log2(N) = log2(N*(N+1)/2), which is O(N*log2(N)).
    First of all, the binary function takes O(log2(N)) time.
    In each step of the loop from the function insert we have O(log2(N)) time compexity from binary, and then
  in the worst case scenario, when the initial array is sorted decreasingly, we need pos operations to insert the element because we need to shift the array to the right every time,
  so the compexity is linear, and overall, as the while loop is executed N times, the time compexity is O(N^2).
  */
  // Some tests:
  def main (args: Array[String]) =
    {
      val a1 = Array (1,1,1,1,1)
      assert (insert(a1).sameElements(a1))
      val a2 = Array (1,2,3,4,5)
      assert (insert(a2).sameElements(a2.sorted))
      val a3 = Array (5,4,3,2,1)
      assert (insert(a3).sameElements(a3.sorted))
      val a4 = Array (1,2,3,3,1,2,3,2,2,1,1,3)
      assert (insert(a4).sameElements(a4.sorted))
      val a5 = Array (2)
      assert (insert(a5).sameElements(a5.sorted))
    }
}

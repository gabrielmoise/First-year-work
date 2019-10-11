object Question7
{
  /** Testing if there exists an i in [0..N) that satisfies p*/
  def exists(p : Int => Boolean, N : Int): Boolean =
    {
      var i = 0
      // Invariant I : there is no element in [0..i) that satisfies p && 0<=i<=N
      // variant N-i
      while (i < N)
      {
        // I && 0<=i<N
        if (p(i)) return true
        // If we returned true then we found a value that satisfies p
        // If not, then p(i) didn't hold, so p(i) = false
        // I && p(i) false => there is no elemtent in [0..(i+1)) that satisfies p && 0<=i<N
        i += 1
        // I : there is no element in [0..i) that satisfies p && 0<=i<=N
      }
      // If we didn't return true yet, because I holds we have no element in [0..N) that satisfies p, so we return false
      false
    }
  def main (args: Array[String]) =
    {
      val prop1: (Int => Boolean) = i => {i%5==0}; val n1 = 20
      assert (exists(prop1,n1) == true)
      val prop2: (Int => Boolean) = i => {i%5==0}; val n2 = 0
      assert (exists(prop2,n2) == false)
      val prop3: (Int => Boolean) = i => {i%11==0 && i!=0}; val n3 = 10
      assert (exists(prop3,n3) == false)
      val prop4: (Int => Boolean) = i => {i*i>=400}; val n4 = 21
      assert (exists(prop4,n4) == true)
      val prop5: (Int => Boolean) = i => {i*i>=400}; val n5 = 20
      assert (exists(prop5,n5) == false)
    }
}

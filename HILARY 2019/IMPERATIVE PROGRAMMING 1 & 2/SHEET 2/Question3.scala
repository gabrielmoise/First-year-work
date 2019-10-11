object Question3
{
  /* Function that two arrays are the same or not*/
  def equal (a: Array[String], b: Array[String]) : Boolean =
    {
      val n = a.size
      val m = b.size
      // Obviously, if the arrays are of different sizes, they cannot be equal
      if (n!=m) return false
      var i = 0
      // Invariant I: a[0..i) = b[0..i) && 0<=i<=n
      while (i<n)
      {
        // 0<=i<n
        if (a(i) != b(i)) return false
        // If we got past this point, then a(i) == b(i), so we have a[0..(i+1)) = b[0..(i+1))
        i += 1
        // I
      }
      // I holds, so a[0..n) = b[0..m) (as m=n), so the two arrays are identical
      true
    }
    // This function is an order that I defined:
    // String s1 is smaller then s2 if s1 reversed is smaller than reversed s2
    def ord (s1: String, s2: String) : Boolean =
      {
          // We turn the 2 strings into arrays
          val a = s1.toArray
          val b = s2.toArray
          // We reverse their elements
          val reva = a.reverse
          val revb = b.reverse
          // We turn them back into strings
          val revs1 = new String (reva)
          val revs2 = new String (revb)
          // We check if revs1 is less than or equal to revs2 and we return the answer
          revs1 <= revs2
      }
    def main (args : Array[String]) =
      {
        val test1 = Array ("banana","apple","college","professor","dog","cat")
        val sort1 = test1.sorted
        val result1 = Array ("apple","banana","cat","college","dog","professor")
        assert (equal(sort1,result1))
        val test2 = Array ("aaaa","aa","aaa","aaaaaa","a","aaaaa")
        val sort2 = test2.sorted
        val result2 = Array ("a","aa","aaa","aaaa","aaaaa","aaaaaa")
        assert (equal(sort2,result2))
        val order : ((String,String) => Boolean) = (s1,s2) => {ord(s1,s2)}
        val test3 = Array ("banana","apple","college","professor","dog","cat")
        val sort3 = test3.sortWith(order)
        val result3 = Array ("banana","college","apple","dog","professor","cat")
        assert (equal(sort3,result3))
        val test4 = Array ("a","a","a","a","a","a","a")
        val sort4 = test4.sortWith(order)
        val result4 = Array ("a","a","a","a","a","a","a")
        assert (equal(sort4,result4))
      }
}

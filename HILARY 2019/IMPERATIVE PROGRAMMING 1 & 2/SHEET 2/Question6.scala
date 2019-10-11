object Question6
{
  /** For a given value n, check if after the first n characters, s starts repeating itself*/
  def equal (n: Int, N: Int, s: Array[Char]) : Boolean =
    {
      var i = 0
      // Invariant s[0..i)=s[n..i+n) && 0<=i<=N-n
      // Variant N-n-i
      while (i<N-n)
      {
        if (s(i) != s(i+n)) return false else i += 1
        // If we find that two characters differ then we return false, else we continue by increasing i by 1, so in the loop, if we don't get out by returning false, the invariant holds
      }
      // At the end, if we haven't returned false, then it means that we have i=N-n and from the invariant we have that
      // s[0..N-n) = s[n..N), so we return true
      true
    }
    /** Calculating the number of characters in s after which it starts repeating itself*/
  def search (s : Array[Char]) : Int =
    {
      val N = s.size
      var n = 1
      // Invariant : the period of s is greater or equal to n
      // variant : N-n
      while (n<=N)
        if (equal (n,N,s)) return n else n +=1
        // If after the first n characters s starts repeating itself, then we return n, else we increase n and we continue checking
      0
      // We will never return 0 because for n=N, equal(n,N,s) will always be true, so the result is always less than or equal to N
    }
  def main (args: Array[String]) =
    {
      val test1 = Array ('a','a','a','a')
      assert(search(test1) == 1)
      val test2 = Array ('a','b','c','d','e')
      assert(search(test2) == 5)
      val test3 = Array ('a')
      assert(search(test3) == 1)
      val test4 = Array ('a','b','a','b','a','b','a','b')
      assert(search(test4) == 2)
      val test5 = Array ('a','b','c','d','a','b')
      assert(search(test5) == 4)
    }
}

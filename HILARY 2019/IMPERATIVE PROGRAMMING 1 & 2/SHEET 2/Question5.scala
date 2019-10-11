object Question5
{
  /** Does pat appear as a substring of line?
  * i.e. pat [0..K) = line [i..i+K) for some i in [0..j). */
  def search (pat: Array[Char], line: Array[Char]) : Boolean =
    {
      val K = pat.size ; val N = line.size
      // Invariant I: found = (line[i..i+K) = pat [0..K) for some i in [0..j)) and 0<=j<=N-K
      var j = 0 ; var found = false
      while (j <= N-K && !found)
      {
        // set found if line [j..j+K) = pat [0..K)
        // Invariant : line [j..j+k) = pat [0..k)
        var k = 1
        while (k<K && line(j+k)==pat(k)) k = k + 1
        found = (k==K)
        j = j + 1
      }
      // I && (j=N-K+1 || found)
      // found = (line[i..i+K) = pat [0..K) for some i in [0..N-K+1) )
      found
    }
  def main (args: Array[String]) =
    {
      // (a) If we set found to be true initially, then we won't enter the while loop, so search will always
      // return true for any input. So, we will get an error for any false case, like this one:
      val pat1 = Array ('A') ; val line1 = Array ('B')
      assert (search(pat1,line1) == false)
      // (b) Here, if we replace the <= in the first while condition with <, we will not consider the
      // possibility that pat is the suffix of line, because the suffix of K characters is
      // line[N-K..N), but in order to verify if pat2 is equal to that we would need j to reach N-K inclusively
      val pat2 = Array ('a') ; val line2 = Array ('c','b','a')
      assert (search (pat2,line2) == true)
      // (c) If we replace N-K with N-K+1 in the first while condition we will get
      // ArrayIndexOutOfBoundsException
      // because in the case when j = N-K+1, in the second while when k reaches K-1
      // we will test if line(j+k), meaning line(N) which is where we get out of the array, because line has just
      // N elements, indexed from 0 to N-1 inclusively.

      // (d) This test fails because when we check with the second while if line [j..j+K) = pat [0..K)
      // we start from k = 1 to K-1 and we do not check for the first character. Also, as we can see
      // the Invariant doesn't hold initially before getting into the while loop: line [j..j+k) = pat [0..k)
      // is no longer true if we start with k=1, as line(j) might differ from pat(0).
      val pat3 = Array ('b','o','a','t') ; val line3 = Array ('R','a','i','n','c','o','a','t')
      assert (search (pat3,line3) == false)
      //(e) We get ArrayIndexOutOfBoundsException again because when we get to k = K, pat(k) will be in fact
      // pat(K), which doesn't exist because we got out of the array.
      // (f) If we say found = (k>=K) this won't affect the program in any way because k at each
      // step in the second while loop can increase only by 1, so at the end of the loop, if we found
      // pat in line we will have k=K so found will be true whether we set the condition to be k==K or k>=K, and for k<K we will get false anyways.
    }
}

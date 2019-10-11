object Question8
{
  // (a) As p/q >= 1/m and 1/m is the biggest reciprocal with this property,
  // then m is the smallest integer with this property, we then have m>=q/p, so in order for m to be the smallest
  // with this property, we need m=ceiling(q/p). (Here / is not div, but the fraction sign)
  def ceiling (p : Int, q : Int) : Int =
    {
      // m>=q/p if and only if m*p>=q (we are guaranteed that 0<p<q)
      var m = 1 //m is non-negative as 0<p<q and non-zero because we can't divide by 0
      // We search for m by incrementing it by 1 and testing if we exceeded q/p. When we do, we stop, so
      // m is the smallest integer with m>=q/p, then m=ceiling(q/p) as we needed
      while (m*p<q) m += 1
      m
    }
  /** Decomposing P/Q into a sum of reciprocals*/
  def sum (P : Int, Q : Int) : Array[Int] =
    {
      var p = P ; var q = Q
      var n = 10000
      var a = new Array [Int] (n) // We assume that n is sufficiently large for all the reciprocals we want to calculate
      var k = 0 // The current number of reciprocals we have found to be part of the sum
      var m = 1
      // Once we found a reciprocal, we continue from the m we found, we do not try again from 1 because p/q - 1/m becomes smaller so the next
      // m we will find will be greater than or equal to the previous one (we'll discuss the "equal to" part later)
      // Invariant I: p/q+sum(1/a(i))=P/Q with i in [0..k)
      // variant p (we'll prove that it decreases after every iteration of the loop)
      while (p!=0)
      {
        // We find the reciprocal with minimum m
        while (m*p<q) m += 1
        // We found a reciprocal so we put it in the array
        a(k) = m ; k += 1
        // Now, we will update p and q with p1 and q1 such that p1/q1 = p/q - 1/m = (p*m-q)/(q*m)
        val p1 = p*m - q
        // (c) We are assured that p1<p because
        // p*m-q<p <=> p*(m-1)<q <=> (m-1)<q/p and we chose m to be the smallest integer with m*p>=q, or m>=q/p, therefore (m-1)<p/q,
        // therefore p1<p so p decreases every time. Also p/q cannot get negative since we never substract from p/q something bigger than it,
        // so p>=0 and q>0 (q remains positive so p must also remain at least zero) therefore p will eventually reach zero
        val q1 = q*m // proof that q is always positive as we claimed
        p = p1 ; q = q1
        // The invariant I holds since the new fraction p/q is smaller with 1/m than the previous one
        // and the sum of the elements of the array (whose size increased by 1) is greater with 1/m
      }
      // p becomes 0, so we get 0/q+the sum of the first k elements from a = P/Q, so we return the array which contains the
      // first k elements of a (as those are the ones we are interested in, the rest are only zeros)
      val result = new Array [Int] (k)
      var i = 0
      while (i<k) {result(i) = a(i); i+=1}
      result
      // (d) Let's suppose that there are two values in result that are equal (from our algorithm they have to be on consecutive positions as m is at least as big as the previous one)
      // Then, at some point we subtracted 1/m from p/q and then again 1/m from (p/q-1/m). That means that we could have
      // subtracted 2/m from p/q, thus we could have subtracted 1/(ceiling(m/2)) from p/q, as ceiling(m/2)>=m/2. Then, the while loop would have stopped at ceiling(m/2) and add that to the result instead,
      // thus we can't have two equal elements in the result array.
    }
    /*
    scala> Question8.sum(25,26)
    res1: Array[Int] = Array(2, 3, 8, 312)
    scala> Question8.sum(1,26)
    res2: Array[Int] = Array(26)
    scala> Question8.sum(48,96)
    res3: Array[Int] = Array(2)
    scala> Question8.sum(3,8)
    res4: Array[Int] = Array(3, 24)
    */
}

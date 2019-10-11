object Question1
{
/** Find index i s.t. a[0..i) < x <= a[i..N).
* Pre: a is sorted. */
def search(a: Array[Int], x: Int) : Int =
  {
    val N = a.size
    // invariant I: a[0..i) < x <= a[j..N) && 0 <= i <= j <= N
     var i = 0; var j = N
     while(i < j)
     {
       val m = i + (j-i) / 2 //val m = (i+j)/2
       // i <= m < j
       if(a(m) < x) i = m+1 else j = m
     }
     // I && i = j, so a[0..i) < x <= a[i..N)
     i
   }
}
/**
(a) If the array is not increasing we cannot be sure that we get the right answer whether x is in the array or not.
We make comparisons between the element from the middle of the interval we constrained our search, but that doesn't
mean anything if the array is not in increasing order, because the element can be anywhere now regardless of the comparisons. So, based on comparisons,
we increase the left limit or decrease the right one until we get to a point which can be or not the element we are looking for
(the probability to find the good answer is very very small).
scala> Question1.search(Array(1,2,3,8,5,6,7),6)
res5: Int = 3
(b) In N is zero, or in other words the array is empty, we set i to be 0 and j to be 0 so we don't enter the while loop,
therefore we simply return i, which is 0.
(c) The problem appears when (i+j) is greater than or equal to 2^31, but we can fix this by replacing the line
val m = (i+j)/2 with val m = i + (j-i)/2, which is equivalent to that, so that we don't exceed the Int range.
*/

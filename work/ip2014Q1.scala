object Question1
{
  var a = Array (1,2,3,4,5,6,7,8)
  var N = a.size
  var p = 3

  // (a)
  /** a[l..k) < p <= a[k..r) && returns k */
  def partition (l : Int, r : Int, p : Int) : Int = {
    var i = l
    var j = r
    // Invariant I : a[l..i) < p && p <= a[j..r) && l <= i <= j <= r
    // Variant : (j-i)
    while (i < j)
    {
      if (a(i) < p) i += 1
      else {var t = a(i) ; a(i) = a(j-1) ; a(j-1) = t ; j -= 1}
    }
    // i == j => a[l..i) < p <= a[i..r), so we return i
    return i
  }

  val k = partition(0,N,p)

  /** finds a missing value in the interval knowing that the elements a[l..r)
      are all in the interval [low..high) */
  def missing (left : Int, right : Int, low : Int, high : Int) : Int = {
    var l = left
    var r = right
    var min = low
    var max = high
    // Invariant I : there is a value that is missing from the interval [min..max)
    // in the array a[l..r) && min <= a[l..r) < max && l <= r
    // Variant : (r-l)
    while (l < r) {
      var p = (min + max) / 2
      var k = partition(l,r,p)
      if (k < p) {r = k ; max = p}
            else {l = k ; min = p}
    }
    // l == r => there is a value that is missing from the interval [min..max)
    // in the array a[l..r), which is empty, so min is missing from a
    return min
  }

  def main (args : Array[String]) = {
    println (Question1.missing(0,N,0,N+1))
  }
}

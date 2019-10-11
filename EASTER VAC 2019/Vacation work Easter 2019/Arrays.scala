object Question1 {
  def findArray2 (a : Array[Int], b : Array[Int]) : (Int, Boolean) = {
    var N = a.size
    var M = b.size
    var i = 0
    var j = 0
    var ok = false // have we found a common element yet?
    // res is going to be the smallest value common to a and b, if there is one
    // Invariant I : a[0..i) < res <= a[i..N) && b[0..j) < res <= b[j..M)
    // && 0 <= i < N && 0 <= j < M && ok = whether we have found a common element or not
    while ((i < N) && (j < M))
    {
      // I
      if (a(i) == b(j)) return (a(i),true)
      else if (a(i) < b(j)) i += 1 // res has to be bigger than a(i), otherwise we would have found a(i) = b(j0), with j0<j at an earlier step
      else if (a(i) > b(j)) j += 1 // same reasoning here
    }
    // Because of the invariant, we cannot get out of bounds in the loop
    // If we get out of the loop, that means we have not printed anything, so there is no common value in the 2 arrays
    (0,false)
  }

  def findArrays (as : Array[Array[Int]]) : (Int, Boolean) = {
    var N = as.size
    var n = new Array[Int](N)
    for (i <- 0 until N) n(i) = as(i).size
    var index = new Array[Int](N)
    for (i <- 0 until N) index(i) = 0
    var ok = true
    // Invariant I : as(i)[0..index(i)) < res <= as(i)[index(i)..n(i)] &&
    // 0 <= index(i) < n(i) for all i in [0..N) && ok = whether there might
    // a common element in the N arrays or not
    while (ok)
    {
      var minIndex = -1
      var min = as(0)(index(0))
      for (i <- 1 until N) if (min > as(i)(index(i))) {min = as(i)(index(i)) ; minIndex = i}
      var maxIndex = -1
      var max = as(0)(index(0))
      for (i <- 1 until N) if (max < as(i)(index(i))) {max = as(i)(index(i)) ; maxIndex = i}
      if (as(minIndex)(index(minIndex)) == as(maxIndex)(index(maxIndex)))
          {
            ok = false // that means that all the values are equal, so we can stop
            return (as(minIndex)(index(minIndex)),true)
          }
      else
          {
            index(minIndex) += 1 // that means that we need to go to the next element in the array that contains the minimum value
            if (index(minIndex) >= n(minIndex)) ok = false // that means that we got out of bounds, so we need to stop
          }
    }
    (0,false)
    // This means we haven't found a common element among the arrays
  }
}

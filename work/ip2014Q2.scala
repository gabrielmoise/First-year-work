object Question2 {
  var a = Array(Array(false,true,true,true,true,true,true,false,true,true),
                Array(false,true,true,true,true,true,true,false,true,true),
                Array(true,true,false,true,true,true,false,true,true,false),
                Array(true,true,true,true,false,true,true,true,false,true),
                Array(false,true,true,true,false,false,true,true,false,false),
                Array(false,true,true,true,false,true,true,true,true,true),
                Array(true,true,true,true,true,false,true,true,true,false),
                Array(true,true,false,true,false,true,true,true,true,true),
                Array(true,false,true,false,true,true,true,true,true,true),
                Array(false,false,true,true,true,false,true,true,true,true))

  def count : Array[Array[Int]] = {
    var N = a.size
    var rowCount = new Array[Array[Int]](N)
    for (i <- 0 until N)
    {
      var nr = 0
      var line = new Array[Int](N)
      // Invariant I : nr is the number of consecutive true values up to j (on line i)
      // When we get to a true, we increase the current number of consecutive trues
      // and when we get to a  we reset that count
      for (j <- 0 until N)
        if (a(i)(j) == true) {nr += 1 ; line(j) = nr}
            else {nr = 0 ; line(j) = 0}
      rowCount(i) = line
    }
    return rowCount
  }

  def maxRectangleAt(rowCount : Array[Array[Int]], i : Int, j : Int) : Int = {
    var N = rowCount.size
    // first we will make the array min, with min(k) = min {rowCount(m)(j) | m in [i..k)}
    // we have the relation min(i+1) = rowCount(i)(j) and
    // min(k) = minimum between rowCount(k-1)(j) and min(k-1)
    var min = new Array[Int](N+1)
    min(i+1) = rowCount(i)(j)
    // O(N)
    // Invariant : we have calculated the first (k-i-1) values of min according to our recursion rule
    for (k <- i+2 to N)
      if (min(k-1) < rowCount(k-1)(j)) min(k) = min(k-1)
          else min(k) = rowCount(k-1)(j)
    var max = 0
    // O(N)
    // Invariant : max is the maximum area up to the line k as the bottom of the rectangle
    for (k <- i+1 to N)
      if ((k-i) * min(k) > max) max = (k-i) * min(k)
    return max
  }

  // O(N^3)
  def area(rowCount : Array[Array[Int]]) : Int = {
    var N = rowCount.size
    var max = 0
    for (i <- 0 until N)
      for (j <- 0 until N)
        {
          var current = maxRectangleAt(rowCount,i,j)
          if (max < current) max = current
        }
    return max
  }

  def main (args : Array[String]) = {
    var rowCount = count
    var N = rowCount.size
    for (i <- 0 until N)
    {
      for (j <- 0 until N) print(rowCount(i)(j)+" ")
      println("\n")
    }
    //println(maxRectangleAt(rowCount,5,8))
    /*
    var min = maxRectangleAt(rowCount,5,6)
    for (i <- 6 until N+1) print(min(i)+" ")
    println("\n")
    */
    println(area(rowCount))
  }
}

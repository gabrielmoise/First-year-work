object Question2
{
  def binary (y: Int) : Int =
    {
      // Trivial cases:
      if (y==0) return 0
      if (y==1) return 1
      // Invariant I: a^2 <= y < b^2 and 0 <= a < b
      var a = 0; var b = y // problems with overflow at part (c)
      while(a+1 < b)
      {
        var m = (a+b)/2 // a < m < b
        if  (m <= Math.pow(y,0.5)) a = m
            else b = m
      }
      // a^2 <= y < (a+1)^2
      a
    }
  /** Pre : integer y > 0
      Post : integer a >= 0 such that a^2 <= y < (a+1)^2 */
  def ternary (y : Int) : Int =
    {
      // We first return the special cases y=0 and y=1 so that by starting with
      // right=y we can say that right^2>=y (by setting right=y+1 we have a problem at part (c) with the overflow)
      if (y==0) return 0
      if (y==1) return 1
      var left = 0 ; var right = y
      // Invariant I : left^2 <= y < right^2 && 0 <= left < right <= y
      // Variant (right-left)
      while (left + 2 < right)
      {
        // right - left >= 3 => (right-left) / 3 >= 1
        var midLeft = left + (right-left) / 3
        var midRight = right - (right-left) / 3
        // midLeft < midRight <=> left + (right-left) / 3 < right - (right-left)/3 <=> 2*((right-left)/3) < right-left (which is true since right-left>=3)
        // (b) left < midLeft < midRight < right, so we notice that we only work with intervals that are non-empty !!
        if (Math.pow(y,0.5) < midLeft) right = midLeft // We don't use y<midLeft*midLeft for part (c) where we can overflow
        else if (Math.pow(y,0.5) < midRight) {left = midLeft ; right = midRight}
        else left = midRight
        // I
      }
      // I && ! (left+2<right) => 1 <= (right-left) <= 2
      var result = 0
      if (right == left + 1) result = left
         else
         {
           // left + 2 = right
           var middle = left + 1
           if (Math.pow(y,0.5) < middle) result = left //left * left <= y < middle * middle
              else result = middle // middle * middle <= y < right * right
         }
      result
    }
  def main (args: Array[String]) =
    {
      // (a) && (c) checked up to Int.MaxValue - 1 (so also for up tp 46000 and up to 10^8)
      for (i <- 0 to 2147483647) assert (binary(i) == ternary(i))
    }
}

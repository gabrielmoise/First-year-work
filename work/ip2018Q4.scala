class ArrayDoubleEndedQueue(val MAX: Int) extends DoubleEndedQueue {

  private var a = new Array[Int](MAX)
  private var left = 0
  private var right = 0
  private var size = 0

// Abstraction function : queue = {a(i) | if (left < right) i in [left..right) else
// if (left > right) i in [0..right) U [left..MAX), else if size = 0 i in {}, else
// i in [0..MAX)}
// DTI : 0 <= left,right < MAX && size = (right - left) % MAX if right != left
// or size = 0 if left = right and the queue is empty or size = MAX is the queue is full

  def isEmpty : Boolean = (size == 0)

  def addLeft (x : Int) : Unit =
    {
      if (left == 0) left = MAX-1
      else left -= 1
      a(left) = x
      size += 1
    }

  def getLeft : Int = {
    var res = a(left)
    if (left == MAX-1) left = 0
    else left += 1
    size -= 1
    return res
  }

  def addRight (x : Int) : Unit =
    {
      a(right) = x
      if (right == MAX-1) right = 0
      else right += 1
      size += 1
    }

  def getRight : Int = {
    if (right == 0) right = MAX-1
    else right -= 1
    var res = a(right)
    size -= 1
    return res
  }

  /** prints a representation of the current abstract datatype to the screen */
  // Post : print every element of the array a from a(left) until a(right) 
  def display : Unit = {
      for (i <- left until left + size) print(a(i%MAX)+" ")
      println()
  }
}

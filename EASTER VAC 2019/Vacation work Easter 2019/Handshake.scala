class Handshake extends IntSet {
  private var MAX = 1000
  private var loc = new Array[Int](MAX)
  for (i <- 0 until MAX) loc(i) = -1
  private var value = new Array[Int](MAX)
  var size = 0

  // x is in the set if its location exists and if we have
  // at least loc(x) + 1 elements in value, otherwise it may be
  // there, but the array has been cleared at a previous step
  def contains (x : Int) : Boolean = (loc(x) != -1) && (size > loc(x)) && (value(loc(x)) == x)

  def insert (x : Int) = if (contains(x) == false){
    loc(x) = size
    value(size) = x
    size += 1
  }

  // we delete x and replace its location with the last elements from value
  def delete (x : Int) = if (contains(x)) {
    value(loc(x)) = value(size-1)
    loc(value(size-1)) = loc(x)
    loc(x) = -1
    size -= 1
  }

  def clear () = size = 0
}

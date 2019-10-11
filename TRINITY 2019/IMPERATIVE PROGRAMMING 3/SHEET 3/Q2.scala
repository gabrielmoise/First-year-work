trait PartialOrder[T] {
  def <=(that: T): Boolean // checks this <= that. Partial order on T.
  def lub(that: T): T // returns the least upper bound of this and that.
}

class MySet[T] (elements : Set[T]) extends Set[T] with PartialOrder[MySet[T]]{
  // I use a pre-defined List as my concrete implementation
  private val mySet : List[T] = elements.toList
  def contains (key : T) : Boolean = {
    for (i <- this ; if (key == i)) return true
    false
  }
  def iterator : Iterator[T] = mySet.iterator
  def + (elem : T) : MySet[T] = new MySet (mySet.toSet + elem)
  def - (elem : T) : MySet[T] = new MySet (mySet.toSet - elem)
  override def empty : MySet[T] = new MySet(Set())

  // We can use for (v <- this/that) because we already created the method iterator
  def <= (that : MySet[T]) : Boolean = {
    for (v <- this) if (! that.contains(v)) return false
    true
  }
  def lub (that : MySet[T]) : MySet[T] = {
    var result = this
    for (v <- that) result = result + v
    result
  }
}

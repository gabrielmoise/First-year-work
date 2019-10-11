trait PartialOrder[T] {
  def <=(that: T): Boolean // checks this <= that. Partial order on T.
  def lub(that: T): T // returns the least upper bound of this and that.
}

/** My concrete class implementation is based on a singly-linked list */
class MySet[T] (elements : Set[T]) extends PartialOrder[MySet[T]] with Set[T] {
  /** Initialisation */
  private class Node(val datum : T, var next : Node)
  private var mySet : Node = null
  var it = elements.iterator
  while (it.hasNext) {this.+(it.next)}
  /** Iterator method */
  override def iterator : Iterator[T] = {
    var it = new Iterator[T] {
      var current = mySet
      def hasNext() : Boolean = (current != null)
      def next() : T = {
        var result = current.datum
        current = current.next
        return result
      }
    }
    return it
  }
  /** Contains method */
  override def contains (key : T) : Boolean = {
    var current = mySet
    while (current != null)
    {
      if (current.datum == key) return true
      current = current.next
    }
    return false
  }
  /** (+) method */
  override def + (elem : T) : MySet[T] = {
    if (this.contains(elem)) return this
    var n1 = new Node(elem,mySet)
    mySet = n1
    this
  }
  /** (-) method */
  override def - (elem : T) : MySet[T] = {
    var current = mySet
    if (current.datum == elem) {mySet = mySet.next ; this}
    var prev = current
    if (current != null) current = current.next
    while (current != null)
    {
      if (current.datum == elem) {prev.next = prev.next.next ; return this}
      current = current.next
      prev = prev.next
    }
    return this
  }
  /** Empty method */
  override def empty : MySet[T] = {
    var emptySet : MySet[T] = new MySet[T](Set())
    return emptySet
  }
  /** <= method */
  override def <= (that : MySet[T]) : Boolean = {
    var it = this.iterator
    while (it.hasNext) if (that.contains(it.next) == false) return false
    return true
  }
  /** Lub method */
  override def lub (that : MySet[T]) : MySet[T] = {
    var result : MySet[T] = that
    var it = this.iterator
    while (it.hasNext)
    {
      var x = it.next
      if (that.contains(x) == false) result.+(x)
    }
    return result
  }
}

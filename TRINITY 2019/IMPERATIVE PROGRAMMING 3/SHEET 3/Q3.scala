class UpSet [T <: PartialOrder[T]](_elements : Set[T]) extends PartialOrder[UpSet[T]]{
  /** We create the set using a "minimal" standard set */
  private var elements = minimal(_elements)

  /** Check if the value is in the set */
  def contains (x : T) : Boolean = {
    for (v <- elements ; if (v <= x)) return true
    false
  }

  /** Intersect two upsets */
  def intersection (that : UpSet[T]) : UpSet[T] = {
    var result = Set[T] ()
    for (a <- elements ; b <- that.elements) result = result + (a lub b)
    new UpSet(result)
  }

  /** Private function that finds the minimal subset */
  private def minimal (data : Set[T]) : Set[T] = {
    var result : Set[T] = data
    for (a <- data) {
      var ok = true
      for (b <- data ; if ((a != b) && (b <= a))) ok = false
      if (! ok) result = result - a
    }
    result
  }

  /** Compare this with that */
  def <= (that : UpSet[T]) : Boolean = {
    for (v <- elements ; if (! that.contains(v))) return false
    true
  }

  /** Find the lub of two sets (reunion) */
  def lub (that : UpSet[T]) : UpSet[T] = {
    var result = elements
    for (v <- that.elements) result = result + v
    new UpSet(result) // automatically finds the minimal subset
  }
}

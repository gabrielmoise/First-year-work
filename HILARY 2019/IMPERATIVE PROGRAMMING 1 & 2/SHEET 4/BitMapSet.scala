class BitMapSet
{

  // Abs : set = {x ∈ [0..N) | a(x) = true}
  // DTI : a(i) = true <=> i ∈ set for all 0 <= i < N && count = # {a(i) = true | 0 <= i < N}

  // The number of elements currently in the stack
  private var count = 0
  // The biggest element that can appear in the set
  private val N = 10000
  // The Boolean-type array of size N
  private val a = new Array [Boolean] (N)
  // Setting every element to false as initially the set is empty
  for (i <- 0 until N) a(i) = false

  // Adds elem to the set by setting a(elem) to true (if elem was already in the set nothing changes)
  // Pre : 0 <= elem < N
  // Post : a(i) = a0(i) for all 0 <= i < N with i/=elem, and a(elem) = true and count += 1 if a0(elem) = false
  def add (elem : Int) =
    {
      require (0 <= elem && elem < N)
      if (!a(elem)) count += 1
      a(elem) = true
    }

  // The value of a(elem) tells whether elem is in the set or not
  // Pre : 0 <= elem < N
  // Post : a = a0 (we don't modify the array in any way)
  def isIn (elem : Int) : Boolean = a(elem)

  // Removes elem from the set by setting a(elem) to false (if elem was not in the set nothing happens)
  // Pre : 0 <= elem < N
  // Post : a(i) = a0(i) for all 0 <= i < N with i/=elem, and a(elem) = false and if a0(elem) = true, count -= 1
  def remove (elem : Int) =
    {
      require (0 <= elem && elem < N)
      if (a(elem)) count -= 1
      a(elem) = false
    }

  // Counts the values of true among the boolean-typed array a
  // Post : count
  def size : Int = count
}

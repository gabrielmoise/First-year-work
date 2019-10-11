// Abs : set = {value(i) | loc(value(i)) = i , value(i) ∈ [0..MAX), i ∈ [0..size)}
// DTI : if (loc(x) = -1) || (size <= loc(x))=> value [0..MAX) != x
//          else value(loc(x)) = x
//       0 <= value[0..size) < MAX && size = # loc(i) != -1, i ∈ [0..MAX)
//       0 <= size <= MAX && -1 <= loc(i) < size, i ∈ [0..MAX)

trait IntSet {

  // State : set ∈ P(N)
  // Init : set = {}

  /* test whether x is a member of the set **/
  // Pre : 0 <= x < MAX
  // Post : return (loc(x) != -1)
  def contains (x : Int) : Boolean

  /* insert x into the set **/
  // Pre : 0 <= x < MAX && size < MAX
  // Post : size = size_0 + 1 &&
  //        loc(x) = size - 1 && value(size - 1) = x
  def insert (x : Int)

  /* delete x from the set **/
  // Pre :  0 <= x < MAX 
  // Post : size = size_0 - 1 && loc(x) = -1 && value(loc(x)_0) = 0
  def delete (x: Int)

  /* make the set empty **/
  // Pre : there is no pre-condition
  // Post : size = 0 && loc[0..MAX) = -1 && value[0..MAX) = 0
  def clear()
}

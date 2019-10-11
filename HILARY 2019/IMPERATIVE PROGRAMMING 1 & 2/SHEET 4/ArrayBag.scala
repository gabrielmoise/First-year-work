// The concrete state represents the mapping bag = {bag(0), bag(1) ...bag(MAX-1)}
// Abstract function : bag = {i -> bag(i) | 0 <= i < MAX}
// DTI : bag(i) = the number of times i appears in the array for all 0 <= i < MAX

object ArrayBag extends Bag{
  private val MAX = 10000 // the maximum value of an integer that is put in the bag (actually that is MAX-1)
  private val c = new Array [Int] (MAX)

  // As the initial state suggests, the c array contains only 0 in the beginning :
  for (i <- 0 until MAX) c(i) = 0

  // Add the element x to the bag
  // Pre : 0 <= x < MAX
  // Post : c(x) increases by 1 and c(i) remains the same for all 0 <= i < MAX with i /= x
  def add (x : Int) = c(x) += 1

  // Find the number of copies of x in the bag
  // Pre : 0 <= x < MAX
  // Post : the c array remains the same and we return c(x)
  def copies (x : Int) : Int = c(x)
}

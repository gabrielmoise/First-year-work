// The interface of the bag

// Each implementation of this trait represents a mapping from integers to the number of times they appear in the bag

trait Bag {
  // state : bag : Int -> Int
  // init : bag = {0->0 ,1->0, 2->0, 3->0... (MAX-1)->0} , or bag(x) = 0 for all 0 <= x < MAX 

  // Add the element x to the bag
  // Pre : 0 <= x < MAX
  // Post : bag(x) increases by 1 and bag(i) remains the same for all 0 <= i < MAX with i /= x
  def add (x : Int)

  // Find the number of copies of x in the bag
  // Pre : 0 <= x < MAX
  // Post : the bag array remains the same and we return bag(x)
  def copies (x : Int) : Int

}

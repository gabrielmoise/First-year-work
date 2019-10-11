// Representing the phone book using a pair of arrays

object ArraysBook extends Book{
  private val MAX = 1000 // max number of names we can store
  private val names = new Array[String](MAX) // which will be ordered lexicographically
  private val numbers = new Array[String](MAX) // the corresponding array
  private var count = 0
  // Abs : These variables together represent the mapping { names(i) -> numbers(i) | i <- [0..count) }
  // DTI : count <= MAX && entries in names[0..count) are in a strictly-increasing order

  // pre: names is sorted
  // post: the index i < count such that names(i) = name or return count if no such index exists
  // Time Complexity : O(log2(count))
  private def find(name: String) : Int = {
    var left = 0 ; var right = count
    // Invariant: names[0..left) < name <= names[right..count)
    while (left < right)
    {
      val middle = (left + right) / 2 // left <= middle < right
      if (names(middle) < name) left = middle + 1
          else right = middle
    }
    // left == right => names[0..left) < name <= [left..count), but we have to check if the name appears in the array, or not
    // Therefore, we must check that names(left) = name:
    if (name == names(left)) left
        else count // if not, as we said, we return count
  }

  // Time complexity : O(log2(count))
  /** Return the number stored against name */
  def recall(name: String) : String = {
    val i = find(name)
    assert(i<count)
    numbers(i)
  }

  // Time complexity : O(log2(count))
  /** Is name in the book? */
  def isInBook(name: String) : Boolean = find(name)<count

  // Here, we wil implement a binary search, and there are 2 possibilities:
  // 1. name is an element of names and we therefore update the number with the new one
  // 2. name is not an element of names, but we find left such that names[0..left) < name <= names[left..count), therefore we
  // will insert name there with the corresponding number
  // Time complexity : O(count) for the insertion part (the search is o(log2(count))
  def store(name: String, number: String) = {
    var left = 0 ; var right = count
    // Invariant: names[0..left) < name <= names[right..count)
    while (left < right)
    {
      val middle = (left + right) / 2 // left <= middle < right
      if (names(middle) < name) left = middle + 1
          else right = middle
    }
    // left == right => names[0..left) < name <= [left..count) and we do what we described above:
    if (name == names(left)) numbers(left) = number
    // Otherwise, to insert an element in the position left, we have to shift all the elements from left,left+1...count-1 by one
    // and increase count by 1
        else if (count<MAX)
        {
          var i = count
          // names[left+1..count+1) = names0[left..count) && numbers[left+1..count+1) = numbers0[left..count)
          while (i>left) {names(i) = names(i-1) ; numbers(i)=numbers(i-1) ; i -= 1}
          names(left) = name ; numbers(left) = number
          count += 1
        }
  }

  // We first search if name is in names and then we delete it by shifting the elements from the right by one position to the left and
  // decrease count by 1, otherwise we simply return false
  // Time complexity : O(count) for the shifting part
  def delete (name : String) : Boolean ={
    var pos = find(name)
    if (pos<count)
    {
      var i = pos
      // names[pos..count-1) = names0[pos+1..count) && numbers[pos..count-1) = numbers0[pos+1..count)
      while (i<count-1) {names(i) = names(i+1) ; numbers(i) = numbers(i+1) ; i += 1}
      count -= 1
      true
    }
     else false
  }
}

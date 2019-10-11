// Representing the phone book using a pair of arrays

object ArraysBook extends Book{
  private val MAX = 1000 // max number of names we can store
  private val names = new Array[String](MAX)
  private val numbers = new Array[String](MAX)
  private var count = 0
  // These variables together represent the mapping
  // { names(i) -> numbers(i) | i <- [0..count) }
  // invariant: count <= MAX &&
  // entries in names[0..count) are distinct

  // Return the index i<count s.t. names(i) = name; or
  //              return count if no such index exists
  private def find(name: String) : Int = {
    // Invariant: name not in names[0..i) && i <= count
    var i = 0
    while(i < count && names(i) != name) i += 1
    i
  }

  /** Return the number stored against name */
  def recall(name: String) : String = {
    val i = find(name)
    assert(i<count)
    numbers(i)
  }
  /** Is name in the book? */
  def isInBook(name: String) : Boolean = find(name)<count


  /** Add the maplet name -> number to the mapping */
  def store(name: String, number: String) = {
    val i = find(name)
    if(i == count){
      assert(count < MAX); names(i) = name; count += 1
    }
    numbers(i) = number
  }

  /** Delete the number stored against name (if it exists) and return whether the name was in the phone book or not */
  def delete(name: String) : Boolean =
    {
      val pos = find (name) // finding the position of name in names
      if (i!=count)
      {
        count -= 1
        names(i) = names(count)
        numbers(i) = numbers(count)
        true
      }
      else false
    }
}

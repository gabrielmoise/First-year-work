// A hash table, representing a bag of words; i.e., for each word we record
// how many times the word is stored.

object ArrayBag{
  private class Node (val word : String, var count : Int)
}

// DTI : table(hash(word)) = Node(word,count) if it was null when we added the word, or table(pos) = Node(word,count) where pos is the first null position found
// starting from hash(word) and wrapping around at MAX && there are size_ words in the table and size_ <= MAX.
// So, basically we have between hash(word) and pos all places non-null. Also, a node containing word can only appear once in table!

class ArrayBag{
  // The hash function we will use
  private def hash(word: String) : Int = {
    def f(e: Int, c: Char) = (e*41 + c.toInt) % MAX
    word.foldLeft(1)(f)
  }

  private val MAX = 100
  private var size_ = 0 // # distinct words stored


  private val table = new Array[ArrayBag.Node](MAX)

  // Given a word and h = hash(word), this function finds the index in table where word is or if it is not, the first  empty place to put
  // the word in(if we find an empty space, then after it there cannot be any appearance of the word we are looking for as that word would
  // have been placed in the first empty space found the previous time). We can also find a place where table(n).count = 0, meaning there
  // was a number of appearances of a specific string which got deleted until count became 0, so we can reuse that space.
  private def find (word: String, h : Int) : Int =
    {
      var n = h
      while ((table(n) != null) && (table(n).word != word) && (table(n).count != 0)) n = (n+1) % MAX
      n
    }

  /** Add an occurrence of word to the table */
  def add(word: String) = {
    val h = hash(word)
    var n = find(word, h)
    if ((table(n) == null) || (table(n).count == 0)) table(n) = new ArrayBag.Node (word, 1) // word was not in table or got deleted
       else table(n).count += 1 // word was already in table
    size_ += 1
  }

  /** The count stored for a particular word */
  def count(word: String) : Int = {
    val h = hash(word)
    val n = find(word, h)
    if (table(n) != null) return table(n).count
       else return 0
  }

  // return the size
  def size = size_

  /** Deletes word if it is in the bag and returns true if the word was found in the bag and false otherwise*/
  def delete(word: String) : Boolean=
    {
      val h = hash(word)
      var n = find(word,h)
      if ((table(n) != null) && (table(n).count > 0))
        {
          table(n).count -= 1
          return true
        }
      else return false
    }
}

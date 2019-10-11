class Trie {
  private val children = new Array [Trie] (26)
  private var complete = false

  private def toIndex (c : Char) : Int = c.toInt - 'a'.toInt
  private def toChar (i : Int) : Char = (i + 'a'.toInt).toChar

  // (a)
  def contains (w : String) : Boolean = {
    var i = toIndex(w(0))
    if (w.size == 1)
    {
      if ((children(i) != null) && (children(i).complete == true)) return true
                                  else return false
    }
    else
    {
      if (children(i) != null) return children(i).contains(w.tail)
          else return false
    }
  }

  // (b)

  def add (w : String) : Unit = {
    var i = toIndex(w(0))
    if (w.size == 1)
    {
      if (children(i) == null) children(i) = new Trie
      children(i).complete = true
    }
    else
    {
      if (children(i) == null) children(i) = new Trie
      children(i).add(w.tail)
    }
  }

  // (c)

  /*
  val dict = new Trie
  for (i <- 0 until N) dict.add(word(i))
  */

  def allWords (ls : String) : List[String] = {
    var list = List[String]()
    list
  }

}

object Question8
{
  var dict = new Dictionary("knuth_words.txt")

  // (a) The program is very slow on long strings because the time complexity is O(n^n) and the memory O(n!) for the list
  class List
  {
    case class Node (var word : String, var next : Node)

    var list = new Node ("?", null)

    var end = list

    // We add a new node at the end of the list
    def add (word : String) =
      {
        var n1 = new Node(word,null)
        end.next = n1
        end = n1
      }

    // We get a word from the head of the list, deleting its node
    // Pre : the list is not empty
    def get : String =
      {
        var word = list.next.word
        if (list.next == end) {list.next = null; end = list} // we only change end if the list consisted of one element which was removed
            else list.next = list.next.next
        word
      }
    override def toString : String =
      {
        var str = "{"
        var current = list.next
        while (current != null)
        {
          if (current.next != null) str = str + current.word + ", "
             else str = str+current.word
          current = current.next
        }
        str = str + "}"
        str
      }
  }

  def permutations (word : String) : List =
    {
      // In perm, we start with the last letter of the word and then we add a new letter to all the words we created so far (in all the positions)
      // of length ln-1, which will be 2 at the first while-loop step, and add them at the end. Then we repeat until we are left with a list consisting
      // of all the permutations of length word.length
      var N = word.length
      var perm = new List
      var pos = N-1 // the current position of the letter we add to all the words in perm
      perm.add(word.drop(N-1))
      var ln = 1
      // Invariant : perm contains all the possible permutations of word[(N-ln)..N)
      while (ln < N)
      {
        ln += 1
        var ch = word(N-ln)
        // We get nodes from the list, we put ch in each position to form new sub-permutations and we add them to perm, until we get
        // to a node with length ln (one that was added during this while iteration)
        while (perm.list.next.word.length == ln - 1)
        {
          var subword = perm.get
          // We insert ch in every position (we form ln words)
          for (i<-0 until ln) {var newWord = subword.take(i) + ch + subword.drop(i); perm.add(newWord)}
        }
      }
      // Now we have in perm all the permutations of the initial word
      perm
    }

  // given a list of words, it returns the list containing all the words from the list that are in the dictionary
  def spell_check (perm : List) : List =
    {
      var wordsDict = new List // the list that will the correct words (from our dictionary)
      var current = perm.list.next
      while (current != null)
      {
        if (dict.isWord(current.word)) wordsDict.add(current.word)
        current = current.next
      }
      wordsDict
    }

  // (b)

  var anagramsDict = new AnagramsDictionary("knuth_words.txt")

  val sizeDict = anagramsDict.anagrams.size

  // We search the sorted permutation of the given word in the array (we consider a to be the array consisting of only the first entry of each tuple from our initial array)
  def search(x: String) : Int = {
    // invariant I: a[0..i) < x <= a[j..sizeDict) && 0 <= i <= j <= sizeDict
    var i = 0; var j = sizeDict
    while(i < j){
      val m = (i+j)/2 // i <= m < j
      if(anagramsDict.anagrams(m)._1 < x) i = m+1 else j = m
    }
    // I && i = j, so a[0..i) < x <= a[i..N)
    i
  }

  val value = 18 // by searching for anagrams in the anagramDict.anagrams array we found the the max length is 18, so we print those with length 18

  def main (args: Array[String]) =
    {
      val word = scala.io.StdIn.readLine
      // (a)
      println(spell_check(permutations(word)).toString)

      // (b)
      var pos = search(word.sorted)
      print("{"+ anagramsDict.anagrams(pos)._2 + ", ")
      // From pos+1 we search for the anagrams of word
      var j = pos + 1
      while (anagramsDict.anagrams(j)._1 == anagramsDict.anagrams(pos)._1)
      {
        if (anagramsDict.anagrams(j+1)._1 == anagramsDict.anagrams(pos)._1) print(anagramsDict.anagrams(j)._2 + ", ")
        else print(anagramsDict.anagrams(j)._2)
        j = j + 1
      }
      println("}")

      /* Finding the longest anagrams from the knuth dictionary:
      var i = 0
      while (i < sizeDict)
      {
        if ((i < sizeDict - 1) && (anagramsDict.anagrams(i)._1 == anagramsDict.anagrams(i+1)._1))
        {
          if (anagramsDict.anagrams(i)._2.size == value) print(anagramsDict.anagrams(i)._2+" ")
          while ((i < sizeDict - 1) && (anagramsDict.anagrams(i)._1 == anagramsDict.anagrams(i+1)._1))
          {
            if (anagramsDict.anagrams(i)._2.size == value) print(anagramsDict.anagrams(i+1)._2+" ")
            i = i + 1
          }
          if (anagramsDict.anagrams(i)._2.size == value) println()
        }
        else i = i + 1
      }
      */
      // It turns out that the anagrams of length 18 from knuth_words are pathophysiological and physiopathological.
      // It also turns out that the longest set of anagrams is:
      // {least, setal, slate, stale, steal, stela, tales, teals, tesla}
      // which has 9 anagrams
    }
}

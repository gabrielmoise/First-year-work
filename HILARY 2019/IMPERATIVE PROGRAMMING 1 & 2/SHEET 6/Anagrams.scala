class AnagramsDictionary(fname: String){
  /** An array holding the pairs of the order permutations with the words*/

  var words = new Array [(String,String)] (120000) // the maximum number of words from knuth_words

  var count = 0

  /** Initialise anagrammatical dictionary from fname */
  private def initDict(fname: String) = {
    val allWords = scala.io.Source.fromFile(fname).getLines
    def include(w:String) = w.forall(_.isLower)
    for(w <- allWords; if include(w)) {words(count) = (w.sorted,w) ; count += 1}
  }

  initDict(fname)

  var anagrams = new Array [(String,String)] (count) // we create a new array with no empty cells so that we can sort it

  for (i<-0 until count) anagrams(i) = words(i)

  // we sort the anagrammatical dictionary
  anagrams = anagrams.sorted
}

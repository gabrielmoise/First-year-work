/** A simple application to check the spelling of words */

object SpellCheck0{
  /** The dictionary (arguably should be in separate module) */
  val words = new scala.collection.mutable.HashSet[String]

  /** Initialise dictionary from the file file fname */
  def initDict(fname:String) = {
    val allWords = scala.io.Source.fromFile(fname).getLines
    // Should word w be included?
    def include(w:String) = w.forall(_.isLower)
    for(w <- allWords; if include(w)) words += w
    // println("Found "+words.size+" words")
  }

  /** test if w is in the dictionary */
  def isWord(w: String) : Boolean = words.contains(w)

  def main(args: Array[String]) = {
    assume(args.length==1, "Needs one argument")
    val w = args(0)
    initDict("knuth_words.txt")
    if(isWord(w)) println(w+" is a valid word")
    else println("word "+w+" not found")
  }
}

/** A simple application to check the spelling of words */

object SpellCheck{
  def main(args: Array[String]) = {
    assume(args.length==1, "Needs one argument")
    val w = args(0)
    val dict = new Dictionary("knuth_words.txt")
    if(dict.isWord(w)) println(w+" is a valid word")
    else println("word "+w+" not found")
  }
}

/** The Dictionary class - Dictionary.scala*/
class Dictionary(fname: String) {
  private val words = new scala.collection.mutable.HashSet[String]
  initDict

  /** Check if a word is in the dictinary */
  def isWord(w: String) = words.contains(w)

  /** Initialize the dictionary */
  private def initDict = {
    val allWords = scala.io.Source.fromFile(fname).getLines
    def include(w: String) = w.forall(_.isLower)
    for (w <- allWords; if include(w)) words += w
  }
}

/** WordPathsApp.scala */
import scala.swing._
import scala.swing.event._

object WordPathsApp extends SimpleSwingApplication {
  def top = new MainFrame {
    object SourceText extends TextField {columns = 10}
    object TargetText extends TextField {columns = 10}
    object SearchButton extends Button {text = "Search"}
    var solutionLabel = new Label {text = "No solution"}
    contents = new FlowPanel {
      contents += new Label{text = "Start with: "}
      contents += SourceText
      contents += new Label{text = " End with: "}
      contents += TargetText
      contents += SearchButton
      contents += solutionLabel
    }

    /** the class that finds the path */
    val finder = new PathFinder(new Dictionary("knuth_words"))

    /** a thread that finds the specified path */
    class Worker extends Thread {
      override def run {
        val answer = finder.find_path(SourceText.text, TargetText.text)
        println(answer)
        val message = if (answer == Nil) "No solution" else
        answer.toString
        Swing.onEDT {solutionLabel.text = message}
      }
    }
    listenTo(SearchButton)
    reactions += {
      case ButtonClicked(SearchButton) => (new Worker).start
    }
  }
}

/** PathFinder.scala */
/** A class that finds paths in a dictionary */
class PathFinder(dict: Dictionary) {
  private val path = new scala.collection.mutable.HashMap[String,List[String]]
  private val queue = new scala.collection.mutable.Queue[String]

  /** Find a path from source to target */
  def find_path(source: String, target: String): List[String] = {
    if (!dict.isWord(source) || !dict.isWord(target) || source.length != target.length)
      return Nil
    path.clear
    queue.clear
    path.update(source, List(source))
    queue.enqueue(source)
    while (!queue.isEmpty && !path.contains(target)) {
      val word = queue.dequeue
      val len = word.length
      for (i <- 0 until len; c <- 'a' to 'z') {
        if (word.charAt(i) != c) {
          val new_word = word.take(i) + c + word.drop(i + 1)
          if (!path.contains(new_word) && dict.isWord(new_word)) {
            path.update(new_word, new_word :: path(word))
            queue.enqueue(new_word)
          }
        }
      }
    }
    if (path.contains(target)) path(target).reverse
    else Nil
  }
}


class Trie {
    private val children = new Array[Trie](26)
    private var complete = false

    private def toIndex(c: Char): Int = c.toInt - 'a'.toInt
    private def toChar(i: Int): Char = (i + 'a'.toInt).toChar

    def add(w:String) {
        var act = this
        for (c <- w) {
            val go = toIndex(c)
            if (act.children(go) == null)
                act.children(go) = new Trie
            act = act.children(go)
        }
        act.complete = true
    }

    private def run(node: Trie, cnt: Array[Int], pref: String): List[String] = {
        var answer = List[String]()

        if (node.complete) answer = List[String](pref)
        for (i <- 0 until 26) {
            if (cnt(i) > 0 && node.children(i) != null) {
                cnt(i) -= 1
                answer = answer ++ run(node.children(i), cnt, pref + toChar(i))
                cnt(i) += 1
            }
        }

        answer
    }

    def allWords(ls: String): List[String] = {
        var cnt = new Array[Int](26)
        for (c <- ls) cnt(toIndex(c)) += 1
        run(this, cnt, "")
    }
}

object Gabi {
    def include(w: String): Boolean = w.forall(_.isLower)

    def main(args: Array[String]) = {
        var head = new Trie

        val lines = scala.io.Source.fromFile(args(0)).getLines
        for (w <- lines)
            if (include(w) == true)
                head.add(w)
        println(head.allWords(args(1)))
    }
}

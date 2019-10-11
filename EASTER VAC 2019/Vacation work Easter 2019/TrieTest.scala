object TrieTest {
  def main (args : Array[String]) = {
    var t = new Trie
    t.add("gabi")
    t.add("alex")
    assert(t.contains("alex"))
    assert(t.contains("gabi"))
    assert(! t.contains("nebunie"))
  }
}

object HandshakeMain {
  def main (args : Array[String]) = {
    var set = new Handshake
    set.insert(5)
    set.insert(4)
    assert(set.contains(3) == false)
    assert(set.contains(4))
    set.clear()
    set.insert(3)
    set.insert(2)
    assert(set.contains(3))
    assert(set.contains(4) == false)
    set.delete(2)
    set.insert(1)
    assert(set.contains(2) == false)
    assert(set.contains(1))
  }
}

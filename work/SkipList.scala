object Question3 {
  def main (args : Array[String]) = {
    var list = new SkipList
    list.add(5)
    list.add(2)
    list.add(3)
    list.add(4)
    list.add(1)
    list.add(6)
    list.delete(2)
    //list.delete(4)
    //list.delete(5)
    println(list.contains(5))
    println(list.contains(3))
    println(list.contains(2))
    println(list.contains(6))
    var v0 = list.listAtLevelInt(0)
    for (i <- 0 to 5) print(v0(i) + " ")
    println()
    var v1 = list.listAtLevelInt(1)
    for (i <- 0 to 5) print(v1(i) + " ")
    println()
    var v2 = list.listAtLevelInt(2)
    for (i <- 0 to 5) print(v2(i) + " ")
    println()
    var v = list.findPredsInt(2)
    for (i <- 0 to 5) print(v(i) + " ")
    println()
  }
}
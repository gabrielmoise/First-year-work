object Question5{
  def main (args : Array[String]) = {
    var tree = new CountTree

    tree.add("gabi")
    tree.add("alex")
    tree.add("gabi")
    tree.add("mirescu")
    tree.add("andrei")
    tree.add("gabi1")
    tree.add("alex1")
    tree.add("gabi1")
    tree.add("mirescu1")
    //tree.add("andrei1")
    tree.display
    println()
    tree.displayList
  }
}

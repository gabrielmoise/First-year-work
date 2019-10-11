object BagTest {
  def main (args : Array[String]) = {
    var bag = new Bag
    var n1 = new Bag.Node (5,null)
    var n2 = new Bag.Node (10,null)
    var n3 = new Bag.Node (2,null)
    var n4 = new Bag.Node (6,null)
    bag.add(n1)
    bag.add(n2)
    bag.add(n3)
    bag.add(n4)
    println(bag.findMin)
    println(bag.findMin)
    bag.delMin
    println(bag.findMin)
  }
}

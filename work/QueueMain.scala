object Question4 {
  def main (args: Array[String]) = {
    val queue = new ArrayDoubleEndedQueue(4)
    queue.addLeft(10)
    queue.addLeft(20)
    queue.addRight(30)
    println(queue.getLeft)
    queue.addRight(40)
    queue.addRight(10)
    println(queue.getRight)
    queue.addRight(15)
    println(queue.getLeft)
    print("Queue status: "); queue.display
  }
}

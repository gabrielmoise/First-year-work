// Abs : queue = {x in Int}

trait DoubleEndedQueue {

  // State : queue = P(Z)
  // Init : queue = {}

  /** tests whether the queue is empty */
  // Post : return (queue.size == 0)
  def isEmpty : Boolean

  /** adds the integer x to the start of the queue */
  // Pre : queue.size < MAX !!!! this is changed for the fixed array
  // Post : queue = x : queue_0
  def addLeft (x: Int) : Unit

  /** removes the element at the start of the queue and returns it */
  // Pre : queue.isEmpty == false
  // Post : queue = tail queue_0 && return (head queue_0)
  def getLeft : Int

  /** adds the integer x to the end of the queue */
  // Pre : queue.size < MAX !!!! this is changed for the fixed array
  // Post : queue = queue_0 ++ [x]
  def addRight (x: Int) : Unit

  /** removes the element at the end of the queue and returns it */
  // Pre : queue.isEmpty == false
  // Post : queue = init queue_0 && return (last queue_0)
  def getRight : Int
}

class ArrayQueue extends Queue[Int]
{
  val MAX = 100 // max number of pieces of data

  // The implementation using a "circular array"

  // Abstraction function : queue = data [head..(out+ln)) if out+ln < MAX
  //                        queue = data [head..MAX) ++ [0..(head+ln)%MAX) if out+ln>=MAX
  // DTI : 0 <= ln <= MAX
  var data = new Array [Int] (MAX)

  var head = 0 // where the queue begins
  var ln = 0 // the length of the queue

  // If ln < MAX, then we can add x in data()(head+ln)%MAX) and then increase ln by 1, but if we get to ln = MAX, then the queue is full
  // so we cannot add more elements to it
  def enqueue (x : Int) =
    {
      require (ln < MAX) // or we can say require (!isFull)
      data((head+ln)%MAX) = x
      ln = ln + 1
    }

  // The head of the list is data(head) if the list is not empty, and it doesn't exist if ln = 0
  def dequeue : Int =
    {
      require (ln > 0) // or we can say require (!isEmpty)
      val result = data(head)
      head = (head + 1) % MAX
      ln = ln - 1
      result
    }

  // The queue is empty if ln = 0, therefore we have no elements in the queue
  def isEmpty : Boolean = (ln == 0)

  // The queue is full when we get to ln = MAX, therefore we reached the maximum size allowed for the queue
  def isFull : Boolean = (ln == MAX)
}

// The interface of a stack, which represents a sequence of data (in our case, of type A)


trait Stack[A]
{
  // state = stack -> a sequence of data(elements) of type A that are added in the front and removed from the front
  // init = stack = {}

  // Add an element of type A to the stack
  // Post : stack = elem : stack_0 (Haskell notation)
  def push (elem : A)

  // Remove the most-recently added element from stack and return it
  // Pre : stack is not empty, otherwise we throw an exception
  // Post : stack = tail (stack_0) ∧ return head (stack_0) (Haskell notation)
  def pop : A

  // Check if stack is empty or not
  // Post : stack = stack_0 ∧ return true if the stack is empty and false otherwise
  def isEmpty : Boolean
}

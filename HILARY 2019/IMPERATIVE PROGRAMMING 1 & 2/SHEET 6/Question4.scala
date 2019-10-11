object Question4
{
  case class Tree (var word: String, var left: Tree, var right: Tree)

  // (a)
  def printTreeRec (t : Tree, height : Int) : Unit=
    {
      var str = ". " * height
      if (t != null)
      {
        println(str + t.word)
        printTreeRec(t.left,height+1)
        printTreeRec(t.right,height+1)
      }
      else println(str + "null")
    }

  // (b)
  // Implementing a Stack with linked lists:

  object Stack // I received a warning message when I tried to use the Stack from sala.collection, so I created one for myself
  {
    class Node (var tree : Tree, var height: Int, var next : Node)
  }

  class Stack
  {
    var stack = new Stack.Node (null, 0, null)

    // we add at the front of the stack
    // Post : stack = (t,h) : stack_0
    def push (t : Tree, h: Int) =
      {
        var n1 = new Stack.Node(t, h, stack.next)
        stack.next = n1
      }

    // we get an element from the stack from the front and we delete it from the stack
    // Pre : stack is not empty
    // Post : stack = tail(stack_0) && and returns head(stack_0)
    def pop : (Tree,Int) =
    {
      require(stack.next != null)
      var (t,h) = (stack.next.tree, stack.next.height)
      stack.next = stack.next.next
      return (t,h)
     }
    // Checks if the stack is empty or not
    def isEmpty : Boolean = (stack.next == null)
  }

  def printTreeStack (t: Tree, height : Int) : Unit =
    {
      var st = new Stack
      st.push(t, 0)
      while (st.isEmpty == false)
      {
        var (t,h) = st.pop
        if (t == null)
        {
          var str = ". " * h
          println(str + "null")
        }
        else
        {
          st.push(t.right, h+1) // first we print the left and then the right subtree
          st.push(t.left, h+1)
          var str = ". " * h
          println(str + t.word)
        }
      }
    }

  def main (args: Array[String]) =
    {
      var tr = Tree("three", Tree("four", Tree("five",null,null), Tree("six", Tree("seven",
               Tree("one",null,null), null), null)), Tree("two",null,null))
      printTreeStack(tr,0)
    }
}

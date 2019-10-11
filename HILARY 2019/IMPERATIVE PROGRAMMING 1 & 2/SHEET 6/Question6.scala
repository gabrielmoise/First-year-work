object BinaryTreeBag
{
  private class Tree(var word: String, var count: Int, var left: Tree, var right: Tree)

  // I implemented the Stack as for question 4 too, because I get an error when I tried to use the scala.collection.mutable.Stack:
  // warning: class Stack in package mutable is deprecated (since 2.12.0): Stack is an inelegant and potentially poorly-performing wrapper
  // around List. Use a List assigned to a var instead.
  private class Stack
  {
    case class Node (var tree : BinaryTreeBag.Tree, var next : Node)

    var stack = new Node (null, null)

    def push (t : BinaryTreeBag.Tree) =
      {
        var n1 = new Node(t,stack.next)
        stack.next = n1
      }

    def pop : BinaryTreeBag.Tree =
    {
      require(stack.next != null)
      var t = stack.next.tree
      stack.next = stack.next.next
      return t
     }

    def isEmpty : Boolean = (stack.next == null)
  }
}

class BinaryTreeBag
{
  private type Tree = BinaryTreeBag.Tree
  private def Tree(word: String, count: Int, left: Tree, right: Tree) = new BinaryTreeBag.Tree(word, count, left, right)

  private var root : Tree = null

  // (a) Recursive definition for the function size, which adds the count fields of all the nodes from the tree
  private def sizeRec(t: Tree) : Int =
    {
      if (t != null) return t.count + sizeRec(t.left) + sizeRec(t.right)
         else return 0
    }

  // (b) Iterative version of the function size, using a stack to keep track of the parts of the tree still to be considered
  private def sizeIter (t: Tree) : Int =
    {
      var size = 0
      val stack = new BinaryTreeBag.Stack
      stack.push(t)
      // Invariant : We still need to add the count fields of the current node and of its nodes from the left and right subtrees and
      // of the right child (if the current node is the left child of a node)
      while(stack.isEmpty == false)
      {
        var tr = stack.pop
        if (tr != null) {size += tr.count; stack.push(tr.left); stack.push(tr.right)}
            else {}
      }
      size
    }
}

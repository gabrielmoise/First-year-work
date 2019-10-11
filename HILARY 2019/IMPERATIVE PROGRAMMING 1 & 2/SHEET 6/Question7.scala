object BinaryTreeBag
{
  private class Tree(var word: String, var count: Int, var left: Tree, var right: Tree)

  private class Stack
  {
    case class Node (var tree : BinaryTreeBag.Tree, var depth : Int, var next : Node)

    var stack = new Node (null, 0, null)

    def push (t : BinaryTreeBag.Tree, d: Int) =
      {
        var n1 = new Node(t,d,stack.next)
        stack.next = n1
      }

    def pop : (BinaryTreeBag.Tree, Int) =
    {
      require(stack.next != null)
      var (t,d) = (stack.next.tree,stack.next.depth)
      stack.next = stack.next.next
      return (t,d)
     }

    def isEmpty : Boolean = (stack.next == null)
  }
}

class BinaryTreeBag
{
  private type Tree = BinaryTreeBag.Tree
  private def Tree(word: String, count: Int, left: Tree, right: Tree) = new BinaryTreeBag.Tree(word, count, left, right)

  private var root : Tree = null

  // We want to calculate the minimum and the maximum depth of the tree, at any given point

  // (a) Using a recursive function

  private def depthRec (t: Tree) : (Int,Int) =
    {
      if (t == null) return (0,0)
        else
        {
          var (minLeft,maxLeft) = depthRec(t.left)
          var (minRight,maxRight) = depthRec(t.right)
          var min = 0
          if (minLeft < minRight) min = minLeft
              else min = minRight
          var max = 0
          if (maxLeft < maxRight) max = maxRight
              else max = maxLeft
          return (min+1,max+1)
        }
    }

  // (b) Using an iterative function, and making use of a Stack

  private def depthIter (t: Tree) : (Int,Int) =
    {
      var min = 10000000
      var max = 0
      val stack = new BinaryTreeBag.Stack
      stack.push(t,0)
      while (stack.isEmpty == false)
      {
        var (tr,depth) = stack.pop
        if (tr == null) //we reached a leaf
        {
          if (depth < min) min = depth
          if (depth > max) max = depth
        }
        else {stack.push(t.left,depth+1); stack.push(t.right,depth+1)}
      }
      return (min,max)
    }
}

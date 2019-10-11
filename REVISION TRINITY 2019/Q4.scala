//(a)
class Tree (var datum : Int, var left : Tree, var right : Tree) {

  //(b)
  def traversal (t : Tree) : Unit = if (t != null) {
    traversal(t.left)
    print(t.datum + " ")
    traversal(t.right)
  }

  //(c)
  def MakeTree (u : Array[Int], a : Int, b : Int) : Tree = {
    if ((b-a) > 1)
    {
      var mid = (b-a)/2
      new Tree (u(mid), MakeTree(u,a,mid-1), MakeTree(u,mid+1,b))
    }
    else if ((b-a) == 1) new Tree(u(a),null,null)
    else null
  }

  //(d)
  def traversalIt (t : Tree) : Unit = {
    var current : Tree = t
    while (current != null)
    {
      if (current.left == null)
      {
        print(current.datum + " ")
        current = current.right
      }
      else
      {
        var leftTree : Tree = current.left
        while ((leftTree.right != null) && (leftTree.right != current))
          leftTree = leftTree.right

        if (leftTree.right == null)
        {
          leftTree.right = current
          current = current.left
        }
        // restore the original tree otherwise
        else
        {
          leftTree.right = null
          print(current.datum + " ")
          current = current.right
        }
      }
    }
  }
  //(e)
  /** Each vertex is traversed at most 2 times, so the time complexity if O(N) */
}

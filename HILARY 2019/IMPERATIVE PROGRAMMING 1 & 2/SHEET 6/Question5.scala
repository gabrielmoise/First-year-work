object Question5
{
  case class Tree (var word : String, var left : Tree, var right : Tree)

  /** Function that destructively ﬂips the tree t, exchanging left and right throughout */
  // tree.word remains the same, (t.left,t.right) becomes (t.right,t.left) and we recursively flip the two subtrees
  def flip(t: Tree) : Unit =
    {
      if (t != null)
       {
         var leftTree = t.left
         var rightTree = t.right
         t.right = leftTree
         t.left = rightTree
         flip (t.left)
         flip (t.right)
       }
    }
}

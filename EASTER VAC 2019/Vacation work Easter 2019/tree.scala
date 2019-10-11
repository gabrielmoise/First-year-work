object Solver {

    abstract class Tree
    case class Leaf() extends Tree
    case class Fork(le: Tree, ri: Tree) extends Tree

    def preorder(node: Tree, stepi: Int, dest: Array[Boolean]): Int = {
        node match {
            case Leaf() => {dest(stepi) = false; stepi + 1}
            case Fork(le: Tree, ri: Tree) => {
                var step = stepi
                dest(step) = true; step += 1
                step = preorder(le, step, dest)
                step = preorder(ri, step, dest)
                step
            }
        }
    }

    def iter_preorder(node: Tree, dest: Array[Boolean], stk: Array[Tree]): Int = {
         var act = node
         var i = 0
         var stk_len = 0
         var ok = true

         while (ok == true) {
            act match {
                case Leaf() => {
                    dest(i) = false
                    if (stk_len > 0) {
                        stk_len -= 1
                        act = stk(stk_len)
                    } else {
                        ok = false
                    }
                }
                case Fork(le: Tree, ri: Tree) => {
                    dest(i) = true
                    stk(stk_len) = ri
                    stk_len += 1
                    act = le
                }
            }
            i += 1
         }
         i
    }

    def toTree(preord: Array[Boolean], stepi: Int): (Tree, Int) = {
        var step = stepi
        if (preord(step) == false) {
            (Leaf(), stepi + 1)
        } else {
            val left_son = toTree(preord, stepi + 1)
            val right_son = toTree(preord, left_son._2)
            (Fork(left_son._1, right_son._1), right_son._2)
        }
    }

    def main(args: Array[String]) = {
        val sol = new Array[Boolean](100)
        val need = new Array[Tree](100)
        val tree = toTree(Array[Boolean](true, false, true, false, false), 0)._1
        //val aux = preorder(Fork(Leaf(), Fork(Leaf(), Leaf())), 0, sol)
        val aux = iter_preorder(tree, sol, need)


        for (i <- 0 until aux)
            println(sol(i))
    }
}

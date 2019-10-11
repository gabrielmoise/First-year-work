class SparseMatrix(val N : Int){
  type Entry = (Int,Double)
  type Row = List[Entry]

  private var rows = new Array[Row](N)

  //Initialisation
  for (i <- 0 until N) rows(i) = List()

  // DTI : (x,pos) is in rows(i) <=> matrix(i)(pos) = x && x != 0.0 &&
  // for all i : for all (x,pos) in rows(i) x != 0.0 && 0<=pos<N

  def get (i : Int, j : Int) : Double = {
    var list = rows(i).filter(pair => pair._1 == j)
    if (list == List()) return 0.0
        else return list.head._2
  }

  def set (i : Int, j : Int, x : Double) : Unit = {
    var left = rows(i).takeWhile(pair => pair._1 < j)
    var mid = List((j,x))
    var right = rows(i).dropWhile(pair => pair._1 <= j)
    rows(i) = left ::: mid ::: right
  }

  def scale (s : Int) : SparseMatrix = {
    for (i <- 0 until N)
        rows(i) = rows(i).map(pair => (pair._1,s*pair._2))
    this
  }

  def transpose : SparseMatrix = {
    var res = new SparseMatrix(N)
    var i = N
    while (i > 0)
    {
      var a = new Array[Entry](N)
      rows(i-1).copyToArray(a,0,N)
      var j = rows(i-1).length
      while (j > 0)
      {
        var pos = a(j-1)._1
        (i-1,a(j-1)._2) :: res.rows(pos)
        j -= 1
      }
      i -= 1
    }
    res
  }

  def mult (that : SparseMatrix) : SparseMatrix = {
    var mult = new SparseMatrix(N)
    var res = that.transpose
    for (i <- 0 until N)
      for (j <- N-1 to 0 by (-1))
    {
      var a = new Array[Entry](N)
      var b = new Array[Entry](N)
      rows(i).copyToArray(a,0,N)
      res.rows(j).copyToArray(b,0,N)
      var current = 0
      var index1 = 0 ; var index2 = 0
      var sum = 0.0
      while (current < N)
      {
        var x = 0.0 ; var y = 0.0
        if (current == a(index1)._1) {x = a(index1)._2 ; index1 += 1}
        if (current == b(index2)._1) {y = b(index2)._2 ; index2 += 1}
        sum = sum + x*y
        current += 1
      }
      if (sum != 0.0) (j,sum) :: mult.rows(i)
    }
    return mult
  }
}

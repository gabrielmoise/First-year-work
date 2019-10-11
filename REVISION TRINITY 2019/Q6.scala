trait Sequence[A] {
  def length: Int
  def elementAt(i: Int): A
  def insert(i: Int, x: A): Sequence[A]
  def delete(i: Int): Sequence[A]
}

trait Command[T] {
  def execute (target : T) : Option[Change]
}

trait Change {
  def undo()
  def redo()
}

trait Song {
  
}

class SongSuite (private var songs : Sequence[Song])

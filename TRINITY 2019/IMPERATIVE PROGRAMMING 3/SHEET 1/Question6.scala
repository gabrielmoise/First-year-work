trait Transmittable {
  def transmit
}

class Text(init: Int) extends Transmittable {
  // text = buffer[0..gap) ++ buffer[max-len+gap..max)

  private var buffer = new Array[Char](init)
  private var len = 0
  private var gap = 0
  private def max = buffer.length
  def length = len
  def charAt(pos: Int) : Char = {..}
  def insert(pos: Int, ch: Char) = {..}
  def transmit = {..}
  ..

}
class PlaneText extends Transmittable {
  private val _text : Text = new Text()
  // _text = _text.buffer[0.._text.gap) ++ _text.buffer[_text.max-_text.len+_text.gap.._text.max)

  def length = _text.length
  def charAt (pos: Int) : Char = _text.charAt(pos)
  var lstart = new Array[Int](MAXLINES)
  var nlines = 0
  // Invariant: 0 ≤ nlines ≤ MAXLINES
  // Abs: lstart = lstart[0..nlines)
  def insert(pos: Int, ch: Char) {
    _text.insert(pos,ch)
    // update lstart and nlines
  }
  def transmit = {..}
}

def transmit (text : Transmittable) = text.transmit

/*
  This method encourages loose coupling as these 2 classes do not depend closely
of the details of one another, high cohesion because of the close relationship
between the variables of each class, and also avoids inheritance so the Fragile
Base Class problem is solved. Two consequences of this is that we lose
polymorphism and every method from the PlaneText class has to call the corresponding
method from the Text class and this is not time-efficient.
*/

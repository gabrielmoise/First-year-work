import org.scalacheck._
import org.scalacheck.Prop._
object Q_Text extends org.scalacheck.Properties("Text") {

  property("insert at start") =
    forAll { (s: String) =>
      val t = new Text(); t.insert(0, s)
      t.toString() == s }

  property("insert at end") =
    forAll { (s1: String, s2: String) =>
      val t = new Text(s1); t.insert(t.length, s2)
      t.toString() == s1 + s2 }

  // NEW CHECKS
  property("insert somewhere") =
    forAll { (s1: String, s2: String, p: Int) =>
     val t = new Text(s1)
     val pos: Int = (if (p < 0) (s1.length / 2) else pos) % (s1.length + 1)
     t.insert(pos, s2)
     t.toString() == s1.take(pos) + s2 + s1.drop(pos)
  }

   property("delete range") =
     forAll {(s: String, pos:Int, cnt: Int) =>
       var start: Int = (if (pos < 0) 0 else pos) % math.max(1, s.length)
       var elems: Int = (if (cnt < 0) 0 else cnt) % math.max(1, s.length - beg)
       val t = new Text(s)
       t.deleteRange(start, elems)
       t.toString == s.take(start) + s.drop(start + elems)
   }

}

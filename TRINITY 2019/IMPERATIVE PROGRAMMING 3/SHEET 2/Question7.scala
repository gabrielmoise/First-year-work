import org.scalacheck._
import org.scalacheck.Prop._

object Question7 extends org.scalacheck.Properties("Question7") {

  property("is a root") =
      forAll { (n: Int) => {
        var aux : Long  = n
        (scala.math.sqrt(aux*aux) == math.abs(aux)) } }

}

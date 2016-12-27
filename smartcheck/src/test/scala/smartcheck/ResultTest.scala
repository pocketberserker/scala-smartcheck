package smartcheck

import scalaz.std.anyVal._
import scalaprops._

object ResultTest extends Scalaprops {

  import Result._

  implicit def gen[A](implicit A: Gen[A]): Gen[Result[A]] =
    Gen.oneOf(Gen.value(baseType), Gen.value(failedPreCond), Gen.value(failedProp), A.map(satisfied(_)))

  val laws = scalazlaws.monad.all[Result]
}

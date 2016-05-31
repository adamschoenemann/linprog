
package adsc.utils


import scala.language.higherKinds
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

class RationalTests extends FlatSpec with Checkers {

  behavior of "Rational"

  def debug(msg:Any) = ()//println(msg)

  it should "work" in {
    val r1 = Rational(10, 20)
    debug(r1.simplify)
    assert (r1.simplify == Rational(1, 2))

    val r2 = Rational(7, 35)
    debug(r2.simplify)
    assert (r2.simplify === "1/5")

    val r3 = Rational(6, 16)
    debug(r3)
    assert (r3 === "3 / 8")

    val r4 = Rational(14, 21)
    debug(r4)
    assert (r4 === " 2 / 3 ")

    val r5 = Rational(4, 8) + Rational(4, 8)
    debug(r5)
    assert (r5 === 1)

    val r6 = Rational(2, 4) + Rational(4, 8)
    debug(r6)
    assert (r6 === 1)

    val r7 = Rational(6, 3) + Rational(3, 9)
    debug(r7)
    assert (r7 === "21/9")

    val r8 = Rational(11, 7) + Rational(1, 6)
    debug(r8)
    assert (r8 === "73/42")

    val r9 = Rational(1, 6) + Rational(11, 7)
    debug(r9)
    assert (r9 === "73/42")

    val r10 = Rational(1,2) + 1
    debug(r10)
    assert (r10 === "3/2")

    val r11 = 1 + Rational(1,2)
    debug(r11)
    assert (r11 === "3/2")

    val r12 = 2 * Rational(1,2)
    debug(r12)
    assert (r12 === "1")

    val r13 = Rational(5, -3)
    debug(r13.standardize)
    assert (r13.standardize === "-5 / 3")

    debug(Rational.int2rat(-1) * Rational(1,1))
  }
}
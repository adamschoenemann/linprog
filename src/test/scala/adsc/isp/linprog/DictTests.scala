

package adsc.isp.linprog


import scala.language.higherKinds
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

import Dict.syntax._

class DictTests extends FlatSpec with Checkers {

  def debug() = {}//println()
  def debug(msg:Any) = {}//println(msg)

  behavior of "Dict"

  it should "create the right dictionaries in book problem" in {
    val d1 = Dict(List(
      row("x4",
        List(c(4), v(-1, "x1"), v(-1, "x2"), v(-2, "x3"))),
      row("x5",
        List(c(5), v(-2, "x1"), v(-3, "x3"))),
      row("x6",
        List(c(7), v(-2, "x1"), v(-1, "x2"), v(-3, "x3")))
      ), row("z", List(v(3, "x1"), v(2, "x2"), v(4, "x3"))))

    assert (d1.isSatisifed)
    debug(d1); debug()
    debug("d1.entering: " ++ d1.entering.toString)
    debug("d1.leaving:  " ++ d1.leaving.toString)
    debug("\nenter x3, leave x5:")

    val d2 = d1.pivot(enters = "x3", leaves = "x5")
    assert (d2.isSatisifed)
    debug(d2); debug()

    debug("d2.entering: " ++ d2.entering.toString)
    debug("d2.leaving:  " ++ d2.leaving.toString)
    debug("\nenter x2, leave x4:")

    val d3 = d2.pivot(enters = "x2", leaves = "x4")
    debug(d3); debug()

    assert(d3.isSatisifed)

    debug("d3.entering: " ++ d3.entering.toString)
    debug("d3.leaving:  " ++ d3.leaving.toString)
    debug("\nenter x1, leave x3:")

    val d4 = d3.pivot(enters = "x1", leaves = "x3")
    assert(d4.isSatisifed)
    debug(d4)
  }

  it should "yield the right dictionaries when pivoting" in {
    val d1 = Dict(List(
      row("x3",
        List(c(4), v("-1/2", "x1"), v(-1, "x2"), v(1, "x0"))),
      row("x4",
        List(c(4), v(-1, "x1"), v(1, "x0"))),
      row("x5",
        List(c(-2), v(2, "x1"), v(1, "x2"), v(1, "x0")))
      ), row("w", List(v(-1, "x0"))))

    debug(d1)
    debug("\nenter x0, leave x5:")
    val d2 = d1.pivot(enters = "x0", leaves = "x5")
    debug(d2)

    debug("\nd2.entering: " ++ d2.entering.toString)
    debug("d2.leaving: " ++ d2.leaving.toString)
    debug("enter x1, leave x0:")
    val d3 = d2.pivot(enters = "x1", leaves = "x0")
    debug(d3)
  }

  it should "yield the right dictionaries when pivoting (2)" in {
    val d1 = Dict(List(
      row("x4",
        List(c(3), v(-1, "x1"), v(-3, "x2"), v(-1, "x3"))),
      row("x5",
        List(c(2), v(1, "x1"), v(-3, "x3"))),
      row("x6",
        List(c(4), v(-2, "x1"), v(1, "x2"), v(-2, "x3"))),
      row("x7",
        List(c(2), v(-2, "x1"), v(-3, "x2"), v(1, "x3")))
      ), row("z", List(v(5, "x1"), v(5, "x2"), v(3, "x3"))))

    debug(d1)
    debug("\nenter x1, leave x7:")
    val d2 = d1.pivot(enters = "x1", leaves = "x7")
    debug(d2)

    debug("\nenter x3, leave x6:")
    val d3 = d2.pivot(enters = "x3", leaves = "x6")
    debug(d3)

    debug("\nenter x2, leave x5:")
    val d4 = d3.pivot(enters = "x2", leaves = "x5")
    debug(d4)
  }
}

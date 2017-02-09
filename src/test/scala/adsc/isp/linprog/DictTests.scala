

package adsc.isp.linprog


import scala.language.higherKinds
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import adsc.utils.Rational

import Dict.syntax._

class DictTests extends FlatSpec with Checkers {

  def debug() = {}
  def debug(msg:Any) = {}

  // def debug() = println()
  // def debug(msg:Any) = println(msg)

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
    val e1 = d1.entering
    val l1 = d1.leaving
    assert (e1.get.toString == "x3")
    assert (l1.get.toString == "x5")
    debug("d1.entering: " ++ e1.toString)
    debug("d1.leaving:  " ++ l1.toString)
    debug("\nenter x3, leave x5:")

    val d2 = d1.pivot(enters = "x3", leaves = "x5")
    assert (d2.isSatisifed)
    debug(d2); debug()

    val e2 = d2.entering
    val l2 = d2.leaving
    assert (e2.get.toString == "x2")
    assert (l2.get.toString == "x4")

    debug("d2.entering: " ++ e2.toString)
    debug("d2.leaving:  " ++ l2.toString)
    debug("\nenter x2, leave x4:")

    val d3 = d2.pivot(enters = "x2", leaves = "x4")
    debug(d3); debug()

    assert(d3.isSatisifed)

    val e3 = d3.entering
    val l3 = d3.leaving
    assert (e3.get.toString == "x1")
    assert (l3.get.toString == "x3")

    debug("d3.entering: " ++ e3.toString)
    debug("d3.leaving:  " ++ l3.toString)
    debug("\nenter x1, leave x3:")

    val d4 = d3.pivot(enters = "x1", leaves = "x3")
    assert(d4.isSatisifed)
    assert(d4.isFeasible)
    assert(d4.isOptimal)
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

    val e2 = d2.entering
    val l2 = d2.leaving
    assert(e2.get.toString == "x1")
    assert(l2.get.toString == "x0")

    debug("\nd2.entering: " ++ e2.toString)
    debug("d2.leaving: "    ++ l2.toString)
    debug("enter x1, leave x0:")
    val d3 = d2.pivot(enters = "x1", leaves = "x0")
    debug(d3)
    assert(d3.isSatisifed && d3.isFeasible && d3.isOptimal)
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
    assert(d4.isSatisifed && d4.isFeasible && d4.isOptimal)
  }
}

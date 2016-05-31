
package adsc.isp.linprog


import scala.language.higherKinds
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

import Dict.syntax._

class SimplexTests extends FlatSpec with Checkers {

  def debug() = println()
  def debug(msg:Any) = println(msg)

  behavior of "simplex"

  it should "get the right result for the 2013 exam!" in {

    val d1 = Dict(List(
      row("x3",
        List(c(-2), v(2, "x1"), v(1, "x2"), v(1, "x0"))),
      row("x4",
        List(c(3), v(-1, "x1"), v(-1,"x2"), v(1, "x0")))
      ), row("w", List(v(-1, "x0"))))

    // original z
    val z = DictRow("z", List(v(1, "x1")))

    val finalPhase1 = Dict(List(
      row("x1", List(c(1), v("-1/2","x0"), v("-1/2","x2"), v("1/2", "x3"))),
      row("x4", List(c(2), v("3/2","x0"), v("-1/2","x2"), v("-1/2", "x3")))
    ), row("w", List(v(-1,"x0"))))

    val finalPhase2 = Dict(List(
       row("x1", List(c(3), v(-1, "x2"), v(-1, "x4"))),
       row("x3", List(c(4), v(-1, "x2"), v(-2, "x4")))
    ), row("z", List(c(3), v(-1, "x2"), v(-1, "x4"))))

    debug(d1)
    debug("feasible: " + d1.isFeasible)
    assert(d1.isFeasible == false)


    val r1 = Simplex.phase1(d1)
    assert(r1.isRight)

    for {
      d2 <- r1
    } yield {

      println(d2)


      assert (d2 == finalPhase1)


      val primal = Simplex.fromAuxiliary(d2, z, "x0")

      val r2 = Simplex.phase2(primal)
      assert (r2.isRight)
      for {
        d3 <- r2
      } yield {
        println(d3)


        assert (d3 == finalPhase2)
      }
    }

    val bySimpl = Simplex.simplex(d1, "x0", z)
    assert(bySimpl.isRight)
    for { sol <- bySimpl } yield {
      println("By simplex")
      println(sol)
      // assert(sol == finalPhase2)
    }
  }
}
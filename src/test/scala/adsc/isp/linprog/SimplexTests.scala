
package adsc.isp.linprog


import scala.language.higherKinds
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

import Dict.syntax._

import scalaz._
import Scalaz._
import adsc.utils.Rational

class SimplexTests extends FlatSpec with Checkers {

  // def debug() = println()
  // def debug(msg:Any) = println(msg)

  def debug() = ()
  def debug(msg:Any) = () //println(msg)

  behavior of "simplex"

  it should "get the right result for the 2016 exam" in {

    val r = Rational(_,_)

    val d1 = Dict(List(
         row("x4", List(c(8), v(-1,"x1"), v(-1, "x2"), v(1, "x3"))),
         row("x5", List(c(6), v(-1,"x1"), v(1, "x2"))),
         row("x6", List(c(100), v(-1,"x3")))
      ), row("z", List(v(100,"x1"), v(60,"x2"), v(-30,"x3")))
    )

    // debug(d1)

    val \/-(sol) = Simplex.simplex(d1)
    debug("Solution")
    // debug(sol)

    val expected = Dict(List(
      row("x2", List(c(51), v(r(-1,2), "x4"), v(r(1 , 2),"x5"), v(r(-1,2),"x6"))),
      row("x1", List(c(57), v(r(-1,2), "x4"), v(r(-1 , 2),"x5"), v(r(-1,2),"x6"))),
      row("x3", List(c(100), v(-1, "x6")))),
      row("z", List(c(5760), v(-80,"x4"), v(-20, "x5"), v(-50,"x6")))
    )
    assert(sol == expected)
  }
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


    val r1 = Simplex.phase1(d1, 100)
    assert(r1.isRight)

    for {
      d2 <- r1
    } yield {

      debug(d2)

      assert (d2 == finalPhase1)

      val primal = Simplex.fromAuxiliary(d2, z, "x0")

      val r2 = Simplex.phase2(primal, 100)
      assert (r2.isRight)
      for {
        d3 <- r2
      } yield {
        debug(d3)


        assert (d3 == finalPhase2)
      }
    }

    val bySimpl = Simplex.simplex(d1, "x0", z, 100)
    assert(bySimpl.isRight)
    for { sol <- bySimpl } yield {
      debug("By simplex")
      debug(sol)
      // assert(sol == finalPhase2)
    }
  }

  it should "get the right result for exercise1 in exercises12" in {

    import STDProblem.syntax._

    val lp = STDProblem(
      List(v(3, "x1"), v(1, "x2")),
      List(
        constr(List(v(1, "x1"), v(-1,"x2")), -1),
        constr(List(v(-1, "x1"), v(-1,"x2")), -3),
        constr(List(v(2, "x1"), v(1,"x2")), 4)
      )
    )

    println(lp)
    println("dict")
    val dict = lp.asDict
    println(dict)
    println("is feasible? " + dict.isFeasible)

    val aux = lp.auxiliary()
    println("aux")
    println(aux)

    val auxd = aux.asDict("w", "x", -1)
    println("aux dict")
    println(auxd)

    val \/- (auxsol) = Simplex.phase1(auxd, 100)
    println("aux sol")
    println(auxsol)

    println("\nphase2:")
    val phase2d = Simplex.fromAuxiliary(auxsol, dict.objective, "x0")
    val \/- (sol) = Simplex.phase2(phase2d, 100)
    println("solution:")
    println(sol)

  }
}
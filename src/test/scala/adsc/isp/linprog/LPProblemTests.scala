
package adsc.isp.linprog

import scala.language.higherKinds
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import Dict.syntax._

class LPProblemTests extends FlatSpec with Checkers {

  def debug() = {}
  def debug(msg:Any) = {}
  // def debug() = println()
  // def debug(msg:Any) = println(msg)


  it should "give the right dual" in {

    import STDProblem.syntax._
    val lp = STDProblem(
      List(v(4, "x1"), v(1, "x2"), v(5, "x3"), v(3, "x4")),
      List(
        constr(List(v(1, "x1"), v(-1,"x2"), v(-1, "x3"), v(3, "x4")), 1),
        constr(List(v(5, "x1"), v(1,"x2"), v(3, "x3"), v(8,"x4")), 55),
        constr(List(v(-1, "x1"), v(2,"x2"), v(3, "x3"), v(-5, "x4")), 3)
      )
    )

    debug(lp)

    val lpdual = lp.dual
    debug()
    debug("Dual:")
    debug(lpdual)

    debug()
    debug("Dual in stdform:")
    debug(lpdual.toStdForm)
    // val primal = Dict(List(
    //   DictRow("x5",
    //     List(DictConst(1), v(-1, "x1"), v(1, "x2"), v(1, "x3"), v(3,"x4"))),
    //   DictRow("x6",
    //     List(DictConst(55), v(-5, "x1"), v(-1, "x2"), v(-3, "x3"), v(-8, "x4"))),
    //   DictRow("x7",
    //     List(DictConst(3), v(1, "x1"), v(-2, "x2"), v(-3, "x3"), v(5, "x4")))
    //   ), DictRow("z", List(v(4, "x1"), v(1, "x2"), v(5, "x3"), v(3, "x4"))))

    // debug(primal)
    // val dual = primal.dual

    // debug();
    // debug(dual)

  }

  it should "have the right dictionary form" in {
    import STDProblem.syntax._

    val lp = STDProblem(
      List(v(5, "x1"), v(4, "x2"), v(3, "x3")),
      List(
        constr(List(v(2, "x1"), v(3,"x2"), v(1, "x3")), 5),
        constr(List(v(4, "x1"), v(1,"x2"), v(2, "x3")), 11),
        constr(List(v(3, "x1"), v(4,"x2"), v(2, "x3")), 8)
      )
    )

    val dict = Dict(
      List(
        row("x4", List(
          c(5), v(-2, "x1"), v(-3,"x2"), v(-1,"x3")
        )),
        row("x5", List(
          c(11), v(-4, "x1"), v(-1,"x2"), v(-2,"x3")
        )),
        row("x6", List(
          c(8), v(-3, "x1"), v(-4,"x2"), v(-2,"x3")
        ))
      )
    , row("z", List(
        v(5, "x1"), v(4, "x2"), v(3, "x3")
      ))
    )

    val lpdict = lp.asDict
    debug(lpdict)
    assert (lpdict == dict)
  }

  it should "calculate the correct auxiliary problem" in {
    import Relation.syntax._
    import LPProblem.syntax._
    import STDProblem.syntax.{constr => sconstr}

    val prob = STDProblem(
      List(v(1, "x1"), v(-1, "x2"), v(1, "x3")),
      List(
        sconstr(List(v(2, "x1"), v(-1, "x2"), v(2, "x3")), 4),
        sconstr(List(v(2, "x1"), v(-3, "x2"), v(1, "x3")), -5),
        sconstr(List(v(-1, "x1"), v(1, "x2"), v(-2, "x3")), -1)
      )
    )

    debug("original problem")
    debug(prob + "\n")

    val aux = prob.auxiliary()
    debug("auxiliary")
    println(aux)

    val expectAux = STDProblem(
      List(v(-1, "x0")),
      List(
        sconstr(List(v(-1, "x0"), v(2, "x1"), v(-1, "x2"), v(2, "x3")), 4),
        sconstr(List(v(-1, "x0"), v(2, "x1"), v(-3, "x2"), v(1, "x3")), -5),
        sconstr(List(v(-1, "x0"), v(-1, "x1"), v(1, "x2"), v(-2, "x3")), -1)
      )
    )

    assert (aux == expectAux)

  }

  it should "be correct on Spring2015 LP Problem" in {
    import Relation.syntax._
    import LPProblem.syntax._

    val lp = LPProblem(Min,
      List(v(8, "x11"), v(6,"x12"), v(-2, "x21"), v(6, "x22")),
      List(
        constr(List(v(1,"x11"), v(1, "x12")), "<=", 1),
        constr(List(v(1,"x11"), v(1, "x12")), ">=", 1),
        constr(List(v(1,"x21"), v(1, "x22")), "<=", 1),
        constr(List(v(1,"x21"), v(1, "x22")), ">=", 1),
        constr(List(v(1,"x11"), v(1, "x21")), "<=", 1),
        constr(List(v(1,"x11"), v(1, "x21")), ">=", 1),
        constr(List(v(1,"x12"), v(1, "x22")), "<=", 1),
        constr(List(v(1,"x12"), v(1, "x22")), ">=", 1)
      )
    )

    debug(lp)
    val stdform = lp.toStdForm
    debug("\nStandard form:")
    debug(stdform)

    import STDProblem.syntax.{constr => sconstr}

    val correctStd = STDProblem(
      List(v(-8,"x11"), v(-6,"x12"), v(2, "x21"), v(-6, "x22")),
      List(
        sconstr(List(v( 1,"x11"), v( 1, "x12")), 1),
        sconstr(List(v(-1,"x11"), v(-1, "x12")), -1),
        sconstr(List(v( 1,"x21"), v( 1, "x22")), 1),
        sconstr(List(v(-1,"x21"), v(-1, "x22")), -1),
        sconstr(List(v( 1,"x11"), v( 1, "x21")), 1),
        sconstr(List(v(-1,"x11"), v(-1, "x21")), -1),
        sconstr(List(v( 1,"x12"), v( 1, "x22")), 1),
        sconstr(List(v(-1,"x12"), v(-1, "x22")), -1)
      )
    )

    assert (stdform == correctStd)

    val dual = stdform.dual
    debug(dual)

    val correctDual = LPProblem(Min,
      List(v(1,"y1"), v(-1,"y2"), v(1, "y3"), v(-1, "y4"), v(1, "y5"),
           v(-1, "y6"), v(1, "y7"), v(-1, "y8")),
      List(
        constr(List(v(1,"y1"), v(-1,"y2"), v(1, "y5"), v(-1, "y6")), ">=", -8),
        constr(List(v(1,"y1"), v(-1,"y2"), v(1, "y7"), v(-1, "y8")), ">=", -6),
        constr(List(v(1,"y3"), v(-1,"y4"), v(1, "y5"), v(-1, "y6")), ">=",  2),
        constr(List(v(1,"y3"), v(-1,"y4"), v(1, "y7"), v(-1, "y8")), ">=",  -6)
      )
    )

    assert (dual == correctDual)
  }
}

package adsc.isp

import adsc.isp.linprog.Dict
import adsc.isp.linprog.LPProblem
import adsc.isp.linprog.LPProblem.{syntax => lp}
import adsc.isp.linprog.STDProblem
import adsc.isp.linprog.STDProblem.{syntax => std}
import adsc.isp.linprog.Dict.syntax._

object Main extends App {

  def exam2013() = {
    println("run!")
    val d1 = Dict(List(
      row("x3",
        List(c(-2), v(2, "x1"), v(1, "x2"), v(1, "x0"))),
      row("x4",
        List(c(3), v(-1, "x1"), v(-1,"x2"), v(1, "x0")))
      ), row("w", List(v(-1, "x0"))))

    val d2 = d1.pivot(enters = "x0", leaves = "x3")

    println()
    println(d2)
    println("entering: " + d2.entering)
    println("leaving: " + d2.leaving)

    val d3 = d2.pivot(enters = "x1", leaves = "x0")

    println()
    println(d3)

    println("entering: " + d3.entering)
    println("leaving: " + d3.leaving)


    println("phase two:")
    println("========================")
    val d4 = Dict(List(
      row("x1",
        List(c(1), v("-1/2", "x2"), v("1/2", "x3"))),
      row("x4",
        List(c(2), v("-1/2", "x2"), v("-1/2","x3")))
      ), row("z", List(c(1), v("-1/2", "x2"), v("1/2", "x3"))))

    println(d4)
    println("entering: " + d4.entering)
    println("leaving: " + d4.leaving)

    val d5 = d4.pivot(enters = "x3", leaves = "x4")
    println()
    println(d5)
  }

  def exam2012() = {
    val s1 = Dict(List(
       row("x4", List(c(1), v(-1, "x1"), v(-1, "x2"), v(-1, "x3")))
    ), row("z", List(v(1, "x1"), v(1, "x2"), v("1/4", "x3"))))

    println(s1)
    println("entering: " + s1.entering + ", leaving: " + s1.leaving)

    val s2 = s1.pivot(enters = "x1", leaves = "x4")
    println()
    println(s2)
    println("optimal?: " + s2.isOptimal)

  }


  def exam2016() = {
    println("2016")
    val lp = STDProblem(
      List(v(100, "x1"), v(60, "x2"), v(-30, "x3")),
      List(
        std.constr(List(v(1, "x1"), v(1,"x2"), v(-1, "x3")), 8),
        std.constr(List(v(1, "x1"), v(-1,"x2")), 6),
        std.constr(List(v(1, "x3")), 100)
      )
    )
    println(lp)

    val d1 = lp.asDict

    println()
    println("initial dictionary")
    println(d1)

    println()
    println("entering: " + d1.entering.get + ", leaving: " + d1.leaving.get)

    val d2 = d1.pivot(enters = "x1", leaves = "x5")
    println()
    println(d2)

    println()
    println("entering: " + d2.entering.get + ", leaving: " + d2.leaving.get)

    val d3 = d2.pivot(enters = "x2", leaves = "x4")

    println()
    println(d3)

    println()
    println("entering: " + d3.entering.get + ", leaving: " + d3.leaving.get)

    val d4 = d3.pivot(enters = "x3", leaves = "x6")

    println()
    println(d4)

  }

  exam2016();

}
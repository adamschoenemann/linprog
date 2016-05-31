
package adsc.isp

import adsc.isp.linprog.Dict
import adsc.isp.linprog.LPProblem
import adsc.isp.linprog.LPProblem.{syntax => lp}
import adsc.isp.linprog.STDProblem
import adsc.isp.linprog.STDProblem.{syntax => std}
import adsc.isp.linprog.Dict.syntax._

object Main extends App {

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
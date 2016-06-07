# Linear Programming with Simplex
This is a very simple little library for solving linear programming
problems using the Simplex Method. It is _not_ for production purposes, and
certainly extremely unperformant. It is modelled strictly after the treatment
of the Simplex method given in Vasek Chvatal, "Linear Programming".

It is still WIP! But it does not need a lot of work to be complete for my
purposes. However, since I am (hopefully) done with the course, I shan't be
doing more work on this project.

## Features
- Convert from generic (more or less) linear problem to standard form
- Convert from standard form to slack form
- Convert from primal to dual
- Solve pivots and calculate new dictionaries automatically
- No external dependencies (except for testing libraries)

There is WIP implementation of the Simplex algorithm in the `Simplex` namespace,
but for simple problems one can easily manually to do the pivoting, which is
also more controllable.

## Examples

### Solving a STD problem with manual simplex
```scala
import adsc.isp.linprog.Dict
import adsc.isp.linprog.STDProblem
import adsc.isp.linprog.STDProblem.{syntax => std}
import adsc.isp.linprog.Dict.syntax._

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
```

prints the following to console:

    Maximize 100x1 + 60x2 - 30x3
    subject to
        1x1 + 1x2 - 1x3 <= 8
        1x1 - 1x2 <= 6
        1x3 <= 100

    initial dictionary
    x4 = 8 - 1x1 - 1x2 + 1x3
    x5 = 6 - 1x1 + 1x2
    x6 = 100 - 1x3
    -----------------------
    z = 100x1 + 60x2 - 30x3

    entering: x1, leaving: x5

    x4 = 2 - 2x2 + 1x3 + 1x5
    x1 = 6 + 1x2 - 1x5
    x6 = 100 - 1x3
    ------------------------------
    z = 600 + 160x2 - 30x3 - 100x5

    entering: x2, leaving: x4

    x2 = 1 + (1 / 2)x3 - (1 / 2)x4 + (1 / 2)x5
    x1 = 7 + (1 / 2)x3 - (1 / 2)x4 - (1 / 2)x5
    x6 = 100 - 1x3
    ----------------------------
    z = 760 + 50x3 - 80x4 - 20x5

    entering: x3, leaving: x6

    x2 = 51 - (1 / 2)x4 + (1 / 2)x5 - (1 / 2)x6
    x1 = 57 - (1 / 2)x4 - (1 / 2)x5 - (1 / 2)x6
    x3 = 100 - 1x6
    -----------------------------
    z = 5760 - 80x4 - 20x5 - 50x6

Since the final dictionary is optimal, we can read off the solution from it.

### Converting from LP problem to Standard form and Dual problem

```scala

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

println(lp)
val stdform = lp.toStdForm
println("\nStandard form:")
println(stdform)

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

assert(stdform == correctStd)

val dual = stdform.dual
println("Dual problem:")
println(dual)

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

```
This outputs the following:

    Minimize 8x11 + 6x12 - 2x21 + 6x22
    subject to
        1x11 + 1x12 <= 1
        1x11 + 1x12 >= 1
        1x21 + 1x22 <= 1
        1x21 + 1x22 >= 1
        1x11 + 1x21 <= 1
        1x11 + 1x21 >= 1
        1x12 + 1x22 <= 1
        1x12 + 1x22 >= 1
    
    Standard form:
    Maximize -8x11 - 6x12 + 2x21 - 6x22
    subject to
         1x11 + 1x12 <=  1
        -1x11 - 1x12 <= -1
         1x21 + 1x22 <=  1
        -1x21 - 1x22 <= -1
         1x11 + 1x21 <=  1
        -1x11 - 1x21 <= -1
         1x12 + 1x22 <=  1
        -1x12 - 1x22 <= -1
    
    Dual problem:
    Minimize 1y1 - 1y2 + 1y3 - 1y4 + 1y5 - 1y6 + 1y7 - 1y8
    subject to
        1y1 - 1y2 + 1y5 - 1y6 >= -8
        1y1 - 1y2 + 1y7 - 1y8 >= -6
        1y3 - 1y4 + 1y5 - 1y6 >= 2
        1y3 - 1y4 + 1y7 - 1y8 >= -6
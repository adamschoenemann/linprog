
package adsc.isp.linprog

import scala.language.higherKinds
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import Dict.syntax._
import Relation.syntax._
import LPProblem.syntax._
import adsc.utils.Rational
import java.io.PrintWriter

import scalaz._
import Scalaz._

import sys.process._
import scala.language.postfixOps

class ShiftTests extends FlatSpec with Checkers {

  def debug() = {}
  def debug(msg:Any) = {}
  // def debug() = println()
  // def debug(msg:Any) = println(msg)

  case class Day(week:Int, day:Int)
  case class User(name:String, limits: Seq[Int])

  def printSolution(d:Dict):Unit = {
    d.solution
      .map({case (v,x) => (v,x.toDouble)})
      .filter(_._2 > 0)
      .sortBy(_._1)
      .map(println)
  }

  /**
   * nweeks = num weeks
   * diw = days of week
   * nusers = num users
   * types of shifts
   * seed = seed for randomization
   */
  def genShiftProbData(nweeks:Int, diw:Int, nusers:Int, seed:Int = 17) = {

    val weeks = List.range(0,nweeks)
    val days = weeks.flatMap(w =>
      for (d <- w*diw until w*diw+diw) yield
        Day(w, d)
    )
    def daysInWeek(w:Int) = days.filter(_.week == w)


    // val names = Seq("Jonas", "Emma", "Adam", "Sara", "Andreas", "Andrea",
    //                 "Trump", "Anders", "Cecilie", "Jeanette", "Jesper",
    //                 "Greve", "Ditte", "Siri", "Oscar", "Martin", "Soeren"
                   // )

    import scala.util.Random
    val r = new Random(17)
    def mkLimits() = Seq.range(0,4).map(_ => r.nextInt(4) * 10)

    val users = for (i <- 0 until nusers) yield {
      // val nm = names(r.nextInt(names.length)) + i.toString
      val nm = "u" + i.toString
      User(nm, mkLimits())
    }

    (days, users)
  }

  private def writeFile(fn:String, contents:String):Unit = {
    new PrintWriter(fn) { write(contents); close }
  }

  private def mkVar(d:Day, u:User, s:String) = s"${u.name}_${s}_${d.day}"

  def z3FromProblem(days:Seq[Day], users:Seq[User], shifts:Map[String,Rational], diw:Int) = {
    val allVars =
      for {
        d <- days
        u <- users
        s <- shifts.keys
      } yield mkVar(d,u,s)

    // println(allVars)

    val varDecls = allVars.map(v => s"(declare-const ${v} Bool)").mkString("\n")
    val grouped = allVars.grouped(shifts.length).toList

    // each user can only take one shift per day
    val usershiftday = grouped.zipWithIndex
        .map({
          case (g,i) => s"(assert (>= 1 (+ ${g.map(v => s"(b2i ${v})").mkString(" ")})))"
        }).mkString("\n")

    // each shift per day can only be taken by one user
    val shiftdayuser =
      (for { s <- shifts.keys; d <- days } yield (
        users.map(u => mkVar(d,u,s))
      )).zipWithIndex.map({
        case (g,i) => s"(assert (xor ${g.mkString(" ")}))"
      }).mkString("\n")

    // each user must have less hours than his limit each week
    val userweeklimit =
      (for { u <- users; (ds,i) <- days.grouped(diw).zipWithIndex } yield {
        val rowVars =
          for (d <- ds; s <- shifts.keys)
            yield s"(* ${shifts(s).toDouble} (b2r ${mkVar(d,u,s)}))"
        val limit = u.limits(i)
        s"(assert (>= ${limit.toDouble} (+ ${rowVars.mkString(" ")})))"
        // val row = DictRow(s"uwl_${u.name}_${i}", c(limit) :: rowVars.toList)
        // row
      }).mkString("\n")

    val preamble = s"""
    (define-fun b2i ((x Bool)) Int (if (= true x) 1 0))
    (define-fun b2r ((x Bool)) Real (if (= true x) 1.0 0.0))
    """.split("\n").map(_.trim).mkString("\n").init

    val post = "(check-sat)\n(get-model)\n"
    val obj = s"(maximize (+ ${allVars.map(v => s"(b2i ${v})").mkString(" ")}))"

    List(preamble, varDecls, usershiftday, shiftdayuser, userweeklimit, obj, post).mkString("\n\n")

  }
  def dictFromProblem(days:Seq[Day], users:Seq[User], shifts:Map[String,Rational], diw:Int) = {
    val allVars =
      for {
        d <- days
        u <- users
        s <- shifts.keys
      } yield mkVar(d,u,s)

    // println(allVars)

    val grouped = allVars.grouped(shifts.length).toList

    // each user can only take one shift per day
    val usershiftday = grouped.zipWithIndex
        .map({
          case (g,i) => DictRow("usd"+(i+1), c(1) :: g.toList.map(v(-1, _)))
        })

    // each shift per day can only be taken by one user
    val shiftdayuser =
      (for { s <- shifts.keys; d <- days } yield (
        users.map(u => mkVar(d,u,s))
      )).zipWithIndex.map({
        case (g,i) => DictRow("sdu"+(i+1), c(1) :: g.toList.map(v(-1, _)))
      })

    // each user must have less hours than his limit each week
    val userweeklimit =
      (for { u <- users; (ds,i) <- days.grouped(diw).zipWithIndex } yield {
        val rowVars =
          for (d <- ds; s <- shifts.keys)
            yield v(-1 * shifts(s), mkVar(d,u,s))
        val limit = u.limits(i)
        val row = DictRow(s"uwl_${u.name}_${i}", c(limit) :: rowVars.toList)
        row
      })

    val obj = DictRow("z", allVars.toList.map(v(1, _)))

    val d = Dict(usershiftday ++ shiftdayuser ++ userweeklimit, obj)
    d
  }

/*  case class ShiftAssignment(day:Day, shiftType:String, userId:String)
  type LimitStats = Map[String, Rational]
  case class Solution(assignment:Seq[ShiftAssignment], stats:LimitStats)

  def solutionFromDict(dict:Dict):Solution = {
    ???
  }
*/
  behavior of "z3FromProblem"

  it should "generate z3-script from simple problem" in {
    val diw = 2
    val nweeks = 1
    val nusers = 6
    val shifts = Map[String, Rational](
      "x" -> Rational(11,2), "a" -> Rational(13,2),
      "b" -> Rational(19,2)//, "c" -> Rational(13,2), "d" -> Rational(5,1)
    )
    val limits = Seq(
      List.fill (4) (30),
      List.fill (4) (10),
      List.fill (4) (20)
    )
    val users = for (i <- 1 to nusers) yield User("u"+i, limits((i-1)%limits.length))
    // val users = List(User("u1", limits(0)), User("u2", limits(1)), User("u3", limits(2)))
    val (days, _) = genShiftProbData(nweeks, diw, users.length)

    val z3script = z3FromProblem(days, users, shifts, diw)
    println(z3script)
    val fpath = "F:/Dropbox/tmp/z3/input.z3"
    writeFile(fpath, z3script)

    val result = {s"z3 ${fpath}" !!}
    println(result)
  }

  behavior of "dictFromProblem"

  it should "work lolz" in {
    // val (days, users) = genShiftProbData(1, 1, 3)
    // val shifts = Map[String,Double]("x" -> 5.5, "y" -> 4, "z" -> 9)

    // val dict = dictFromProblem(days,users, shifts)
    // println(dict)
  }

  behavior of "Rundetaarn shifts"

  // it should "work with dictFromProblem" in {
  //   val diw = 7
  //   val nweeks = 1
  //   val shifts = Map[String, Rational](
  //     "x" -> Rational(11,2), "a" -> Rational(13,2),
  //     "b" -> Rational(19,2)//, "c" -> Rational(13,2), "d" -> Rational(5,1)
  //   )
  //   val hoursPerWeek = diw * shifts.values.map(_.toDouble).sum
  //   println("hoursPerWeek: " + hoursPerWeek)
  //   val limits = Seq(
  //     List.fill (4) (30),
  //     List.fill (4) (10),
  //     List.fill (4) (20)
  //   )
  //   val users = for (i <- 1 to 30) yield User("u"+i, limits((i-1)%limits.length))
  //   // val users = List(User("u1", limits(0)), User("u2", limits(1)), User("u3", limits(2)))
  //   val (days, _) = genShiftProbData(nweeks, diw, users.length)

  //   val d1 = dictFromProblem(days, users, shifts, diw)

  //   println()
  //   println(d1)
  //   val \/-(sold) = Simplex.phase2(d1, n = 10000)
  //   printSolution(sold)
  // }

  it should "work for a very simple example" in {

    /*
      two users u1, u2, two shifts x0, y0, one day 0:
      u1x0 <= 1
      u2x0 <= 1
      u1y0 <= 1
      u2y0 <= 1
      u1x0 + u1y0 <= 1
      u2x0 + u2y0 <= 1
      u1x0 + u2x0 <= 1
      u1y0 + u2y0 <= 1

      maximize u1x0 + u2x0 + u1y0 + u2y0
    */

    val lp = LPProblem(Max,
      List(v(1, "u1x0"), v(1, "u2x0"), v(1,"u1y0"), v(1, "u2y0")),
      List(
        // constr(List(v(1, "u1x0")), "<=", 1),
        // constr(List(v(1, "u2x0")), "<=", 1),
        // constr(List(v(1, "u1y0")), "<=", 1),
        // constr(List(v(1, "u2y0")), "<=", 1),
        constr(List(v(1, "u1x0"), v(1, "u1y0")), "<=", 1),
        constr(List(v(1, "u2x0"), v(1, "u2y0")), "<=", 1),
        constr(List(v(1, "u1x0"), v(1, "u2x0")), "<=", 1),
        constr(List(v(1, "u1y0"), v(1, "u2y0")), "<=", 1)
      )
    )

    debug("Standard form")
    val stdf = lp.toStdForm
    debug(stdf)
    val dict = stdf.asDict
    val \/-(solDict) = Simplex.phase2(dict, 100)
    debug("Final Dict")
    debug(solDict)
    debug("Solution")
    debug(solDict.solution)


  }

  it should "work for a slightly more advanced example" in {

    def shiftTypes = Map[String,Double](
      "x" -> 5.5
      // "a" -> 6.5,
      // "b" -> 8.5
      // "c" -> 6.5,
      // "d" -> 5
    )

    val shiftKeys = shiftTypes.keys
    val (days, users) = genShiftProbData(1, 1, 1)

    // for each day and shift_type, generate a variable that must be 1
    val assign_every_shift = for {d <- days; t <- shiftKeys} yield {
      List(constr(List(v(1, s"${d.day}")), "<=", 1),
           constr(List(v(1, s"${d.day}")), ">=", 1)
      )
    }
    // val every_user_one_shift_per_day =
    //   for { d <- days, t <- shiftKeys } yield {

    //   }

    /*
      three users u1, u2, u3, three shifts x0, y0, z0, one day 0:
      u1x0 <= 1
      u2x0 <= 1
      u3x0 <= 1
      u1y0 <= 1
      u2y0 <= 1
      u3y0 <= 1
      u1z0 <= 1
      u2z0 <= 1
      u3z0 <= 1
      u1x0 + u1y0 + u1z0 = 1
      u2x0 + u2y0 + u2z0 = 1
      u3x0 + u3y0 + u3z0 = 1
      u1x0 + u2x0 + u3x0 = 1
      u1y0 + u2y0 + u3y0 = 1
      u1z0 + u2z0 + u3z0 = 1

      maximize z = u1x0 + u1y0 + u1z0 +
                   u2x0 + u2y0 + u2z0 +
                   u3x0 + u3y0 + u3z0.
    */

    val lp = LPProblem(Max,
      List(v(1, "u1x0"), v(1, "u1y0"), v(1, "u1z0"),
           v(1, "u2x0"), v(1, "u2y0"), v(1, "u2z0"),
           v(1, "u3x0"), v(1, "u3y0"), v(1, "u3z0")
      ),
      List(
        constr(List(v(1, "u1x0"), v(1, "u1y0"), v(1, "u1z0")), "<=", 1),
        constr(List(v(1, "u2x0"), v(1, "u2y0"), v(1, "u2z0")), "<=", 1),
        constr(List(v(1, "u3x0"), v(1, "u3y0"), v(1, "u3z0")), "<=", 1),
        constr(List(v(1, "u1x0"), v(1, "u1y0"), v(1, "u1z0")), ">=", 1),
        constr(List(v(1, "u2x0"), v(1, "u2y0"), v(1, "u2z0")), ">=", 1),
        constr(List(v(1, "u3x0"), v(1, "u3y0"), v(1, "u3z0")), ">=", 1),

        constr(List(v(1, "u1x0"), v(1, "u2x0"), v(1, "u3x0")), ">=", 1),
        constr(List(v(1, "u1y0"), v(1, "u2y0"), v(1, "u3y0")), ">=", 1),
        constr(List(v(1, "u1z0"), v(1, "u2z0"), v(1, "u3z0")), ">=", 1),
        constr(List(v(1, "u1x0"), v(1, "u2x0"), v(1, "u3x0")), "<=", 1),
        constr(List(v(1, "u1y0"), v(1, "u2y0"), v(1, "u3y0")), "<=", 1),
        constr(List(v(1, "u1z0"), v(1, "u2z0"), v(1, "u3z0")), "<=", 1)
      )
    )

    debug("\n===============================")
    debug("Slightly more advanced example")
    debug("===============================")
    val std = lp.toStdForm
    val c1 = List("u1x0", "u1y0", "u1z0").map(v(-1, _))
    val c2 = List("u2x0", "u2y0", "u2z0").map(v(-1, _))
    val c3 = List("u3x0", "u3y0", "u3z0").map(v(-1, _))
    val c4 = List("u1x0", "u2x0", "u3x0").map(v(-1, _))
    val c5 = List("u1y0", "u2y0", "u3y0").map(v(-1, _))
    val c6 = List("u1z0", "u2z0", "u3z0").map(v(-1, _))

    val allVars =
      List("u1x0", "u1y0", "u1z0",
           "u2x0", "u2y0", "u2z0",
           "u3x0", "u3y0", "u3z0")

    val d1 = Dict(List(
      DictRow("s1", c(1) :: c1),
      DictRow("s2", c(1) :: c2),
      DictRow("s3", c(1) :: c3),
      DictRow("s4", c(1) :: c4),
      DictRow("s5", c(1) :: c5),
      DictRow("s6", c(1) :: c6)
      ),
      DictRow("z", allVars.map(v(1,_)))
    )
    // val d1 = std.asDict

    debug("isFeasible? " + d1.isFeasible)

    val \/-(dsol) = Simplex.phase2(d1, 100)
    debug("\nDict solution: ")
    debug(dsol.solution.map({case (v,x) => (v,x.toDouble.toInt)}).filter(_._2 > 0))

    // debug("\nAuxiliary: ")
    // val aux = std.auxiliary()
    // debug(aux)
    // val \/-(phase1res) = Simplex.phase1(aux.asDict)

    // debug("\nPhase1\n")
    // debug(phase1res)

    // debug("\nFrom auxiliary\n")
    // val sol = Simplex.fromAuxiliary(phase1res, d1.objective, "x0")
    // debug(sol)

    // debug("\nSolution")
    // debug(sol.solution.map({case (v,x) => (v,x.toDouble.toInt)}).filter(_._2 > 0))

  }

}
package adsc.isp.linprog

import scalaz._
import Scalaz._

import adsc.utils.Rational


sealed trait Failure
case class Loops() extends Failure
case class Unbounded() extends Failure
case class Infeasible() extends Failure

object Simplex {

  case class Step(entering:String, leaving:String, dict:Dict)

  type Solution = List[(String, Rational)]
  type Result = (List[Step], Failure \/ Solution)

  def simplexStep(dict:Dict,
    getEntering:Dict => Option[String],
    getLeaving: Dict => Option[String]):Failure \/ Dict = {

    val entering = getEntering(dict)
    val leaving  = getLeaving(dict)
    // println()
    // println(dict)
    // println("entering: " + entering)
    // println("leaving: " + leaving)
    entering match {
      case None => \/- (dict)
      case Some(e) => leaving match {
        case None => -\/ (Unbounded())
        case Some(l) => {
          val d2 = dict.pivot(enters = e, leaves = l)
          \/- (d2)
        }
      }
    }

  }

  // I think this method might rely on you already having made the dict
  // to auxiliary if it is actually infeasible
  @annotation.tailrec
  def phase1(dict:Dict, x0:String = "x0", n:Int = 100):Failure \/ Dict = {
    if (n <= 0) {
      -\/(Loops())
    } else {
      def getEntering(d:Dict):Option[String] = {
        if (d.objective.vars.map(_.id).contains(x0)) {
          Some(x0)
        } else {
          d.entering
        }
      }

      def getLeaving(d:Dict):Option[String] = {
        val zeroed = d.rows.map(r => (r.lhs, r.evalZero))
        zeroed.find(_._2.isNegative).map(_._1).orElse (d.leaving)
      }

      if (dict.isOptimal && dict.isFeasible)
        \/- (dict)
      else {
        simplexStep (dict, getEntering, getLeaving) match {
          case -\/ (fail)  => -\/ (fail)
          case \/- (dict2) => phase1(dict2, n = n - 1)
        }
      }
    }
  }

  def phase2(dict:Dict, n:Int = 100):Failure \/ Dict = {
    if (n <= 0) {
      -\/ (Loops())
    } else {
      def getEntering(d:Dict):Option[String] = {
        d.entering
      }

      def getLeaving(d:Dict):Option[String] = {
        d.leaving
      }

      if (dict.isOptimal && dict.isFeasible)
        \/- (dict)
      else {
        simplexStep (dict, getEntering, getLeaving) match {
          case -\/ (fail)  => -\/ (fail)
          case \/- (dict2) => phase1(dict2, n = n - 1)
        }
      }
    }
  }

  def fromAuxiliary(aux:Dict, z:DictRow, x0:String):Dict = {

    val newRows = aux.rows.map(r => r.copy(rhs = r.rhs.filter(x =>
      if (x.isVar) {
        val v = x.asInstanceOf[DictVar]
        v.id != x0
      }
      else true
    )))

    val zR = newRows.foldLeft (z) ((z2, row) => {
      z2.substitute(row)
    })

    aux.copy(
      rows = newRows,
      objective = zR
    )
  }

  def simplex(dict:Dict, x0:String = "x0", n:Int = 100):Failure \/ Dict = {
    simplex(dict, x0, dict.objective, n)
  }

  // I think this method might rely on you already having made the dict
  // to auxiliary if it is actually infeasible
  def simplex(dict:Dict, x0:String, z:DictRow, n:Int):Failure \/ Dict = {
    if (dict.isFeasible == false) {
      phase1(dict, x0, n) match {
        case -\/ (fail)  => -\/ (fail)
        case \/- (dict2) => {
          phase2(fromAuxiliary(dict2, z, x0), n)
        }
      }
    } else {
      phase2(dict, n)
    }
  }
}
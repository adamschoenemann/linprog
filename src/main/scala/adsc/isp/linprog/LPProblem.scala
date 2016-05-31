package adsc.isp.linprog

import utils._
import adsc.utils.Rational
import scala.language.implicitConversions

sealed trait MaxOrMin
case object Max extends MaxOrMin
case object Min extends MaxOrMin

case class STDConstraint(lhs:List[DictVar], rhs:Rational) {

  def getVarOpt(id:String):Option[DictVar] =
    lhs.find(_.id == id)
}

// A "standard" linear problem with maximization objective and ≤ constraints
case class STDProblem(objective:List[DictVar], constraints:List[STDConstraint]) {

  override def toString:String = {
    val header = "Maximize " + showTerms(objective) + "\n"
    val cHeader = "subject to\n"
    val rows = constraints.map(x => "    " + showTerms(x.lhs) + " <= " + x.rhs)
      .mkString("\n")

    header + cHeader + rows
  }

  def asDict:Dict = asDict("z", "x")
  def asDict(zlab:String, xlab:String):Dict = {
    val newObjective =
        DictRow(zlab, objective)

    val nvars = getVars.length
    val slackRows = constraints.zipWithIndex
      .map({
        case (c,i) => {
          val lhs = xlab ++ (i + nvars + 1).toString
          val rhs = (DictConst(c.rhs)) :: c.lhs.map(_.negate)
          DictRow(lhs, rhs)
        }
      })

    Dict(slackRows, newObjective)
  }

  def getVars:List[String] =
    constraints.map(_.lhs.map(_.id)).flatten.distinct

  def dual:LPProblem = {
    val newBs = objective.map(_.coef)
    val newCs = constraints.map(_.rhs)
    val newObj = newCs.zipWithIndex.map({
      case (c, i) => DictVar(c, "y" + (i+1).toString)
    })

    val newVars = getVars.map(id => {
      val coefs = constraints.map(_.getVarOpt(id))
      coefs.zipWithIndex.map({
        case (co,i) => co.map(c => DictVar(c.coef, "y" ++ (i+1).toString))
      }).filter(!_.isEmpty).map(_.get)
    })

    val newConstraints = newVars.zipWithIndex.map({
      case (r, i) => LPConstraint(r, GEQ, (newBs(i)))
    })
    LPProblem(Min, newObj, newConstraints)
  }

}

object STDProblem {
  object syntax {

    def constr(lhs:List[DictVar], rhs:Rational)
      = STDConstraint(lhs, rhs)

  }
}

sealed trait Relation {
  override def toString:String = this match {
    case LEQ => "<="
    case GEQ => ">="
  }
}
case object LEQ extends Relation
case object GEQ extends Relation

object Relation {
  object syntax {

    implicit def str2rel(s:String):Relation =
      if (s == "<=")
        LEQ
      else if (s == ">=")
        GEQ
      else sys.error("unkown constraint " + s)
  }
}

case class LPConstraint(lhs:List[DictVar], rel:Relation, rhs:Rational) {

  def toStdForm:STDConstraint = {
    if (rel == GEQ)
      STDConstraint(lhs.map(_.negate).asInstanceOf[List[DictVar]], rhs * (-1))
    else
      STDConstraint(lhs, rhs)
  }
}
case class LPProblem(dir:MaxOrMin, objective:List[DictVar], constraints:List[LPConstraint]) {

  def toStdForm:STDProblem = {
    val cs = constraints.map(_.toStdForm)
    val o =
      if (dir == Max) objective
      else objective.map(_.negate).asInstanceOf[List[DictVar]]

    STDProblem(o,cs)
  }

  override def toString:String = {
    val kind = if (dir == Max)
      "Maximize " else "Minimize "
    val header = kind + showTerms(objective) + "\n"
    val cHeader = "subject to\n"
    val rows =
      constraints.map(x =>
          s"    ${showTerms(x.lhs)} ${x.rel} ${x.rhs}")
      .mkString("\n")

    header + cHeader + rows
  }
}

object LPProblem {
  object syntax {
    def constr(lhs:List[DictVar], rel:Relation, rhs:Rational) =
      LPConstraint(lhs,rel,rhs)

  }
}
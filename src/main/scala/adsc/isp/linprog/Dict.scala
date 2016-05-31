
package adsc.isp.linprog

import utils._

import scala.language.implicitConversions
import adsc.utils.Rational.{int2rat}
import adsc.utils.Rational

trait DictTerm {
  def absToString:String = this match {
    case DictConst(c) => c.abs.simplify.toString
    case DictVar(c,v) => c.abs.simplify.toString ++ v.toString
  }
  override def toString:String = this match {
    case DictConst(c) => c.simplify.toString
    case DictVar(c,v) => c.simplify.toString ++ v.toString
  }

  def abs:DictTerm = this match {
    case DictConst(c) => DictConst(c.abs)
    case DictVar(c,v) => DictVar(c.abs, v)
  }

  def isPositive:Boolean = this match {
    case DictConst(v) => v.isPositive
    case (DictVar(c,_)) => c.isPositive
  }

  def /(r:Rational):DictTerm = this match {
    case DictConst(v)     => DictConst(v / r)
    case DictVar(coef,id) => DictVar(coef / r, id)
  }

  def *(r:Rational):DictTerm = this match {
    case DictConst(v)     => DictConst(v * r)
    case DictVar(coef,id) => DictVar(coef * r, id)
  }

  def +(r:Rational):DictTerm = this match {
    case DictConst(v)     => DictConst(v + r)
    case DictVar(coef,id) => DictVar(coef + r, id)
  }

  def negate:DictTerm = this match {
    case DictConst(c)     => DictConst(c * (-1))
    case DictVar(coef,id) => DictVar(coef * (-1), id)
  }

  def isConst:Boolean = this match {
    case DictConst(_) => true
    case _ => false
  }

  def isZero:Boolean = this match {
    case DictConst(x) => x.isZero
    case DictVar(x, _) => x.isZero
  }

  def isVar(v:String):Boolean = this match {
    case DictVar(_, v2) => v == v2
    case _ => false
  }

  def isVar:Boolean = this match {
    case DictVar(_, _) => true
    case _ => false
  }

}
case class DictConst(value:Rational) extends DictTerm
case class DictVar(coef:Rational, id:String) extends DictTerm

case class DictRow(lhs:String, rhs:List[DictTerm]) {

  override def equals(other:Any):Boolean = {
    if (other.isInstanceOf[DictRow]) {
      val otherRow = other.asInstanceOf[DictRow]
      lhs == otherRow.lhs && sortTerms(this.rhs) == sortTerms(otherRow.rhs)
    } else {
      super.equals(other)
    }
  }
  def vars:List[DictVar] = rhs.filter(_.isVar)
    .asInstanceOf[List[DictVar]]

  def consts:List[DictConst] =
    rhs.filter(_.isConst).asInstanceOf[List[DictConst]]

  def hasVar(v:String):Boolean = {
    ! rhs.find ({
      case DictVar(_,id) if id == v => true
      case _ => false
    }).isEmpty
  }

  def setVar(id:String, value:Rational):DictRow =
    copy(rhs = rhs.map(x =>
      if (x.isVar) {
        val varVal = x.asInstanceOf[DictVar]
        if (varVal.id == id)
          DictConst(value * varVal.coef)
        else x
      }
      else x
    ))


  def assign(asment:List[(String,Rational)]):DictRow = {
    asment.foldLeft (this) ((row, x) => row.setVar(x._1, x._2))
  }

  def evalZero:Rational = {
    val as = vars.map(v => (v.id, int2rat(0)))
    eval(as)
  }

  def eval(asment:List[(String,Rational)]):Rational = {
    // println("Assigning " + asment.map(_._1).mkString(",") + " to " + this.rhs)
    evalConsts(assign(asment).rhs.asInstanceOf[List[DictConst]])
  }

  private def evalConsts(xs:List[DictConst]):Rational =
    xs.foldLeft[Rational] (0) ((acc, c) => acc + c.value)

  def isSatisifed:Boolean = {
    evalZero.toDouble >= 0
  }

  def isSatisifed(asment:List[(String,Rational)]):Boolean = {
    eval(asment).toDouble >= 0
  }

  def getVarOpt(v:String):Option[DictVar] = {
      rhs.find ({
        case DictVar(_,id) if id == v => true
        case _ => false
      }).map(_.asInstanceOf[DictVar])
    }

  def getVar(v:String):DictVar = {
    try {
      rhs.find ({
        case DictVar(_,id) if id == v => true
        case _ => false
      }).get.asInstanceOf[DictVar]
    } catch {
      case e:NoSuchElementException =>
        throw new RuntimeException(
          "Could not find variable " ++ v ++ " in row " ++ this.toString)
    }
  }

  def removeVar(v:String):List[DictTerm] = {
    rhs.filter ({
      case DictVar(_,id) if id == v => false
      case _ => true
    })
  }

  def merge(a:List[DictTerm], b:List[DictTerm]):DictRow = {
    // println("MERGIN! " ++ a.toString ++ " with " ++ b.toString)
    val (row,unmerged) =
      a.foldLeft[(List[DictTerm], List[DictTerm])] ((Nil,b)) ((acc, x) => {
        val (merged, toMerge) = acc
        x match {
          case DictConst(c) => {
            val consts = toMerge.filter(_.isConst)
                      .asInstanceOf[List[DictConst]]
            val s =  Rational.sum(consts.map(_.value))
            val y = c + s

            // println("toMerge: " ++ toMerge.toString)
            // println("consts: " ++ consts.toString)
            // println("c: " ++ c.toString)
            // println("s: " ++ s.toString)
            // println("y: " ++ y.toString)
            val rest = toMerge.filter(_.isVar)
            (DictConst(y) :: merged, rest)
          }
          case DictVar(c,v) => {
            val vars = toMerge.filter(_.isVar(v)).asInstanceOf[List[DictVar]]
            val s = Rational.sum(vars.map(_.coef))
            val y = c + s

            // println("toMerge: " ++ toMerge.toString)
            // println("vars: " ++ vars.toString)
            // println("c: " ++ c.toString)
            // println("s: " ++ s.toString)
            // println("y: " ++ y.toString)
            val rest = toMerge.filter(!_.isVar(v))
            (DictVar(y,v) :: merged, rest)
          }
        }
      })
    this.copy(rhs = (row ++ unmerged).filter(! _.isZero))
  }

  def substitute(other:DictRow):DictRow = {
    val subst = other.lhs
    if (this.hasVar(subst) == false)
      this
    else {
      val substVar = this.getVar(subst)
      val otherMulted = other.rhs map (_ * substVar.coef)
      val rhsWithoutVar = this.removeVar(subst)
      // println("\t\t before merge: " ++ (rhsWithoutVar ++ otherMulted).toString)
      val newRow = this.merge(otherMulted, rhsWithoutVar)
      newRow
    }
  }

  def pivot(p:String):DictRow = {
    val pv = getVar(p)
    val newLhs = p
    val extraRhs = DictVar(-1, lhs)
    val newRhs = extraRhs :: this.removeVar(p)
    DictRow(newLhs, newRhs.map(_ / (-1 * pv.coef)))
  }

  override def toString:String = {
    lhs + " = " + showTerms(rhs)
  }

}

case class Dict(rows:List[DictRow], objective:DictRow) {

  def isFeasible = isSatisifed
  def isSatisifed = rows.forall(_.isSatisifed)

  def isOptimal:Boolean = {
    objective.vars.map(_.coef).forall(!_.isPositive) &&
      isFeasible
  }

  def solution:List[(String, Rational)] = {
    objective.vars.map(v => (v.id, int2rat(0))) ++
      rows.map(r => (r.lhs, r.evalZero))
  }

  def getVars:List[String] =
    rows.map(r => r.vars.map(_.id)).flatten.distinct

  def getRow(lhs:String):DictRow =
    rows.find(_.lhs == lhs).get

  def pivot(enters:String, leaves:String):Dict = {
    val pivotedDict = copy(
      rows = rows.map(r => if (r.lhs == leaves) r.pivot(enters) else r)
    )

    val pivoted = pivotedDict.getRow(enters)
    val substRows = pivotedDict.copy(
      rows = pivotedDict.rows.map(r =>
        if (r.lhs == enters) r
        else {
          // println("substituting " + pivoted + " into " + r)
          val sbst = r.substitute(pivoted)
          // println("\tyields: " ++ sbst.toString)
          sbst
        }
      )
    )
    substRows.copy(objective = substRows.objective.substitute(pivoted))
  }

  def entering:Option[String] = {
    val max = objective.vars.reduceLeft ((max, x) => {
      if (x.coef > max.coef)
        x
      else max
    })
    if (max.isPositive)
      Some(max.id)
    else None
  }

  def leaving:Option[String] = entering.flatMap(leaving (_))

  def leaving(entering:String):Option[String] = {
    val l2c = rows.map(r =>
      for {
        h <- r.consts.headOption
        entering <- r.getVarOpt(entering)
        coef <- if (entering.coef < 0) Some(entering.coef) else None
      } yield (r.lhs, h.value / coef)
    )
    // return var with smallest variable
    // println("l2c: " ++ l2c.toString)
    l2c.reduceLeft ((accO, xO) => (accO, xO) match {
      case (None, None)  => None
      case (None, x)     => x
      case (x, None)     => x
      case (Some(acc), Some(x)) => {
        val (v, coef) = acc
        val (v2, coef2) = x
        if (coef2 > coef) Some((v2,coef2))
        else Some(acc)
      }
    }).map(_._1)
  }

  // Not done! Not correct!
  // def dual:DualDict = {
  //   val newBs = objective.vars.map(_.coef)
  //   val newCs = rows.map(_.consts.head)
  //   val newYs = rows.zipWithIndex
  //           .map(((e:DictRow, i:Int) => "y" ++ (i+1).toString).tupled)
  //   val oldVars = getVars
  //   val newVars = oldVars.map(id => {
  //     val coefs = rows.map(_.getVarOpt(id))
  //     coefs.zipWithIndex.map({
  //       case (co,i) => co.map(c => DictVar(c.coef, "y" ++ (i+1).toString))
  //     }).filter(!_.isEmpty).map(_.get)
  //   })

  //   val newRows = newVars.zipWithIndex.map({
  //     case (r, i) => DictRow("y" + (newBs.length + i), DictConst(newBs(i)) :: r)
  //   })

  //   val newObjective = DictRow("w", newCs.zipWithIndex.map({
  //     case (DictConst(c), i) => DictVar(c, "y" ++ (i+1).toString)
  //   }))

  //   DualDict(Dict(newRows, newObjective))
  // }

  override def toString:String = {
    val ostr = objective.toString
    rows.map(_.toString).mkString("\n") ++ "\n" ++
      (List.fill (ostr.length) ('-')) ++
      "\n" ++ ostr

  }
}

object Dict {

  object syntax {

    def v(c:Rational, id:String) = DictVar(c, id)
    def c(v:Rational) = DictConst(v)
    def row(lhs:String, rhs:List[DictTerm]) = DictRow(lhs, rhs)

  }
}




// object Main extends App {

//   import linprog._






//   def testRowSatisfaction() = {
//     val r1 = DictRow("x4", List(DictConst(4), DictVar(2, "x1"), DictVar(3, "x2"), DictVar(-2, "x3")))

//     assert(r1.eval(
//       List(("x1", 1), ("x2", 1), ("x3", 1))
//     ) == int2rat(7))
//     assert(r1.eval(
//       List(("x1", 1), ("x2", 0), ("x3", 2))
//     ) == int2rat(2))
//     assert(r1.eval(
//       List(("x1", -1), ("x2", 0), ("x3", 2))
//     ) == int2rat(-2))
//     assert(r1.isSatisifed(
//       List(("x1", -1), ("x2", 0), ("x3", 2))
//     ) == false)
//     assert(r1.eval(
//       List(("x1", -1), ("x2", 0), ("x3", 1))
//     ) == int2rat(0))
//     assert(r1.isSatisifed(
//       List(("x1", -1), ("x2", 0), ("x3", 1))
//     ) == true)

//     val r2 = DictRow("x3", List(DictConst(-2), DictVar(10, "x1"), DictVar(1, "x2")))
//     assert(r2.isSatisifed == false)


//   }

//   // testExpr()
//   // testRat()
//   // testDict1()
//   // testDict2()
//   // testDict3()
//   // testDual()
//   // testStandard()
//   testRowSatisfaction()
// }
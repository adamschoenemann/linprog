/*
sealed trait Rel
case object Eq extends Rel

case class Equation(e1:Expr, rel:Rel, e2:Expr)

implicit def int2expr(x:Int):Expr =
  Const(x)

implicit def str2expr(x:String):Expr =
  Var(x)

sealed trait Expr {

  def +(e:Expr):Expr = Bin(this, Plus, e)
  def *(e:Expr):Expr = Bin(this, Mult, e)
  def /(e:Expr):Expr = Bin(this, Div,  e)
  def -(e:Expr):Expr = Bin(this, Minus, e)

  override def toString:String = this match {
    case Bin(lhs, op, rhs) => s"($lhs $op $rhs)"
    case Const(x) => x.toString
    case Neg(x) => s"(-$x)"
    case Var(id) => id
  }

  // def collect:Expr = this match {
  //   // WTF is going on here. C'mon you can think of something better when
  //   // you're not tired!?
  //   def canCombine(xs:List[Var], ys:List[Var]):(List[Var],List[Var], List[Var]) = (xs,ys) match {
  //     case (Nil,Nil) => (Nil,Nil,Nil)
  //     case (x, Nil) => (x, Nil, Nil)
  //     case (Nil, x) => (Nil, Nil, x)
  //     case (x :: xs, y :: ys) => {
  //       val (c, ys2) = (y :: ys).foldRight ((Nil,Nil)) { (z, acc) =>
  //         if (z.id == x.id)
  //           (z :: acc._1, acc._2)
  //         else
  //           (acc._1, z :: acc._2)
  //         }

  //     }
  //   }
  //   case Bin(lhs, Plus, rhs) => {
  //     val lvars = lhs.coefficients
  //     val rvars = rhs.coefficients
  //     lvars.foldRight ()
  //   }
  //   case Neg(x) => Neg(x.collect)
  // }
}
case class Const(value: Rational) extends Expr
case class Neg(expr:Expr) extends Expr
case class Bin(lhs:Expr, op:BinOp, rhs:Expr) extends Expr
case class Var(id:String) extends Expr

sealed trait BinOp {
  override def toString:String = this match {
    case Plus => "+"
    case Minus => "-"
    case Mult => "*"
    case Div => "/"
  }

}
case object Plus  extends BinOp
case object Minus extends BinOp
case object Mult  extends BinOp
case object Div   extends BinOp*/
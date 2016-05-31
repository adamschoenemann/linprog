
package adsc.utils

import scala.language.implicitConversions

object Rational {

  def rational(num:Int, denom:Int):Rational = {
    Rational(num,denom)
  }

  implicit def int2rat(x:Int):Rational = {
    rational(x, 1)
  }
  implicit def str2rat(s:String):Rational =
    s.split("/").map(_.trim.toInt) match {
      case Array(n, d) => Rational(n, d)
      case Array(x) => int2rat(x)
    }

  def max(a:Rational, b:Rational) =
    if (a.toDouble < b.toDouble) b else a

  def maximum(xs:List[Rational]):Rational =
    xs.reduce ((a,b) => max(a,b))


  def maxDenom(a:Rational, b:Rational):Rational =
    if (a.denom >= b.denom) a else b

  def minDenom(a:Rational, b:Rational):Rational =
    if (a.denom >= b.denom) b else a

  def extend(a:Rational, b:Rational):(Rational,Rational) = {
    if (a.denom == b.denom)
      (a,b)
    else {
      val a2 = Rational(num = a.num * b.denom, denom = a.denom * b.denom)
      val b2 = Rational(num = b.num * a.denom, denom = b.denom * a.denom)
      (a2,b2)
      // val min = minDenom(a,b)
      // val max = maxDenom(a,b)
      // if (max.denom % min.denom == 0) {
      //   val t = Rational(1, max.denom)
      //   (a.extend(t), b.extend(t))
      // } else {
      //   (a.mult(b.denom), b.mult(a.denom))
      // }
    }
  }

  def sum(rs:List[Rational]):Rational =
    rs.foldLeft[Rational] (0) (_ + _)

}

case class Rational(num:Int, denom:Int) {
  def toDouble:Double = num.toDouble/denom.toDouble

  def toStringSimpl:String = this.simplify.toString

  override def toString:String =
    if (denom == 1) num.toString
    else if (denom == -1) (-num).toString
    else s"($num / $denom)"

  private def extend(t:Rational):Rational = {
    if (t.denom <= denom)
      this
    else if (t.denom % denom == 0)
      Rational(num * (t.denom / denom), t.denom)
    else this
  }

  def mult(x:Int):Rational =
    Rational(num * x, denom)

  def mult(that:Rational):Rational =
    Rational(num * that.num, denom * that.denom)

  def ===(that:Rational):Boolean =
    this.simplify == that.simplify

  def reciprocal:Rational = Rational(denom, num)
  def >(other:Rational):Boolean = this.toDouble > other.toDouble
  def <(other:Rational):Boolean = this.toDouble < other.toDouble

  def abs:Rational = Rational(Math.abs(num), Math.abs(denom))

  def *(x:Int):Rational = this.mult(x)
  def *(that:Rational):Rational = this.standardize.mult(that.standardize)
  def +(that:Rational):Rational = this.standardize.add(that.standardize)
  def /(that:Rational):Rational = this.standardize.mult(that.reciprocal)

  def add(that:Rational):Rational = that match {
    case Rational(num2, denom2) => {
      if (denom2 == denom)
        Rational(num + num2, denom)
      else {
        val (a,b) = Rational.extend(this, that)
        a.add(b)
      }
    }
  }

  def standardize:Rational =
    if (denom < 0)
      Rational(num * -1, denom * -1)
    else this

  def isZero:Boolean =
    num == 0 || denom == 0

  def isPositive:Boolean =
    Math.signum(num) == Math.signum(denom)

  def isNegative:Boolean =
    Math.signum(num) != Math.signum(denom)

  def isNonNegative:Boolean =
    isPositive || isZero

  def simplify:Rational = {
    @annotation.tailrec
    def div2(r:Rational):Rational =
      if (r.num % 2  == 0 && r.denom % 2 == 0)
        div2(Rational(r.num / 2, r.denom / 2))
      else r

    @annotation.tailrec
    def gcd(n:Int, r:Rational):Rational =
      if (n == 1) r
      else if (r.num % n == 0 && r.denom % n == 0)
        Rational(r.num / n, r.denom / n)
      else gcd(n - 1, r)

    try {
      if (denom == 0 || num == 0)
        Rational(0,0)
      else if (num == denom)
        Rational(1,1)
      else if (Math.signum(num) == Math.signum(denom) && Math.signum(num) == -1)
        Rational(Math.abs(num), Math.abs(denom)).simplify
      else if (num > denom) {
        if (num % denom == 0)
          Rational(num / denom, 1)
        else gcd(Math.abs(denom), div2(this))
      }
      else {
        if (denom % num == 0)
          Rational(1, denom / num)
        else gcd(Math.abs(num), div2(this))
      }
    } catch {
      case e:ArithmeticException =>
        throw new ArithmeticException(this.toString ++ ": " ++ e.getMessage())
    }
  }

  override def equals(other:Any):Boolean = {
    if (other.isInstanceOf[Rational]) {
      val other2 = other.asInstanceOf[Rational].standardize.simplify
      val this2 = this.standardize.simplify
      val result = this2.num == other2.num && this2.denom == other2.denom
      // println(s"$this equals $other2 = $result")
      result
    } else {
      super.equals(other)
    }
  }
}

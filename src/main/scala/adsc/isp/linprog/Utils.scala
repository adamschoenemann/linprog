
package adsc.isp.linprog

import adsc.utils.Rational

object utils {

  def sortTerms(terms:List[DictTerm]):List[DictTerm] =
    terms.sortBy({
      case DictConst(_) => ""
      case DictVar(_,v) => v
    })

  def showTerms(terms:List[DictTerm]):String = {

      def rhs2str(rhs:List[DictTerm]):String =
        rhs.foldLeft ("") ((acc,x) => {
          if (x.isPositive)
            " + " + x.absToString + acc
          else
            " - " + x.absToString + acc
        })
      def head2str(h:DictTerm):String =
        if (h.isPositive) h.toString
        else              "-" ++ h.absToString

      val sorted = sortTerms(terms)
      head2str(sorted.head) ++ rhs2str(sorted.tail).mkString
  }
}

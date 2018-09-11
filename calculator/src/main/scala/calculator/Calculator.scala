package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends App{
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    namedExpressions.map {case (k, v) => {
      (k, Signal(eval(k, v(), namedExpressions)))
    }}
  }


  def eval(key: String, expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
      expr match {
        case Literal(value) => value
        case Ref(name) => {
          if(name == key)
            Double.NaN
          else
            eval(getReferenceExpr(name, references), references)
        }
        case Plus(a, b) => eval(a, references) + eval(b, references)
        case Minus(a, b) => eval(a, references) - eval(b, references)
        case Times(a, b) => eval(a, references) * eval(b, references)
        case Divide(a, b) => eval(a, references) / eval(b, references)
      }
    }

    eval(expr, references)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}

package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map(
      {case (name: String, signal: Signal[Expr]) => (name, new Signal(eval(signal(), namedExpressions)))}
    )
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]], referencedNames: Set[String]): Double = expr match {
    case Literal(value) => value
    case Ref(name) =>
      if(referencedNames.contains(name))
        Double.NaN
      else
        eval(getReferenceExpr(name, references), references, referencedNames + name)
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
  }
  
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    eval(expr, references, Set())
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

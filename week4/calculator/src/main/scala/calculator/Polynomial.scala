package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    //Δ = b² - 4ac
    new Signal(b() * b() - 4 * a() * b())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a
    new Signal(Set(
      (-b() + Math.sqrt(computeDelta(a, b, c)())) / (2 * a()),
      (-b() - Math.sqrt(computeDelta(a, b, c)())) / (2 * a())
    ))
  }
}

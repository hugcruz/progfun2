package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    //Δ = b² - 4ac
    new Signal(Math.pow(b(),2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a
    new Signal(
      if(delta() < 0) Set()
      else Set(
        (-b() + Math.sqrt(delta())) / (2 * a()),
        (-b() - Math.sqrt(delta())) / (2 * a())
    ))
  }
}

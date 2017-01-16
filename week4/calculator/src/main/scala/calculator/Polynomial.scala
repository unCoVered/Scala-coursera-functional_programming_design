package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    // We can assign b() to a val.
    Signal((b() * b()) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val minusB = Signal(-1 * b())
    val twoA = Signal(2 * a())
    val sqrt = Signal(math.sqrt(delta()))

    Signal(
      if (delta() < 0) Set()
      else {
        Set(
          (minusB() + sqrt()) / twoA(),
          (minusB() - sqrt()) / twoA()
        )
      }
    )
  }
}

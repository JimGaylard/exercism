object SumOfMultiples {
  def sumOfMultiples(factors: Set[Int], limit: Int): Int = {
    factors.foldRight([])(isMultiple)
  }

  private def isMultiple(x : Int, y : Int) : Boolean = x Mod y == 0
}

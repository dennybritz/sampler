package org.dennybritz.sampler

sealed trait FactorFunction {
  def evaluate(variableValues: List[Double]) : Double
}

case object ImplyFactorFunction extends FactorFunction {
  def evaluate(variableValues: List[Double]) = {

    // Return 0 iff the head is false, or at least one variable in the body is false.
    if (variableValues.size == 1)
      variableValues.head
    else if (variableValues.tail.exists(_ == 0.0))
      1.0
    else if (variableValues.head == 0.0)
      0.0
    else
      1.0

    // This is the "correct" way, but it's probably slow..
    // val notP = variableValues.tail.map(List(_)).map(NotFactorFunction.evaluate)
    // val Q = variableValues.head
    // OrFactorFunction.evaluate(notP :+ Q)
  }
}

case object OrFactorFunction extends FactorFunction {
  def evaluate(variableValues: List[Double]) = {
    if (variableValues.isEmpty || variableValues.exists(_ == 1.0)) 1.0 else 0.0
  }
}

case object NotFactorFunction extends FactorFunction {
  def evaluate(variableValues: List[Double]) = {
    math.abs(variableValues(0) - 1.0)
  }
}
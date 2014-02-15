package org.dennybritz.sampler

sealed trait FactorFunction {
  def evaluate(variableValues: List[Double]) : Double
}

case object ImplyFactorFunction extends FactorFunction {
  def evaluate(variableValues: List[Double]) = {

    // Return 0 iff the head is false, or at least one variable in the body is false.
    if (variableValues.size == 1)
      variableValues.head
    else if (variableValues.take(variableValues.size - 1).exists(_ == 0.0))
      1.0
    else if (variableValues.last == 0.0)
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

case object AndFactorFunction extends FactorFunction {
  def evaluate(variableValues: List[Double]) = {
    if (variableValues.isEmpty || variableValues.forall(_ == 1.0)) 1.0 else 0.0
  }
}

case object EqualFactorFunction extends FactorFunction {
  def evaluate(variableValues: List[Double]) = {
    if (variableValues.size != 2) {
      throw new RuntimeException("Cannot evaluate equality between more than two variables!")
    } else {
      if (variableValues(0) == variableValues(1)) 1.0 else 0.0
    }
  }
}

case object IsTrueFactorFunction extends FactorFunction {
  def evaluate(variableValues: List[Double]) = {
    if (variableValues.size != 1) {
      throw new RuntimeException("Cannot evaluate isTrue for more than one variable!")
    } else {
      variableValues(0)
    }
  }
}

case object NotFactorFunction extends FactorFunction {
  def evaluate(variableValues: List[Double]) = {
    math.abs(variableValues(0) - 1.0)
  }
}
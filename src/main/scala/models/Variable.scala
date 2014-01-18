package org.dennybritz.sampler

case class VariableValue(val value: Double) extends AnyVal

sealed trait Variable {
  def id: Int
  def value: Double
  def isEvidence: Boolean
  def isQuery: Boolean
  def domain: List[Double]
}

case class BooleanVariable(id: Int, value: Double, isEvidence: Boolean, isQuery: Boolean) 
  extends Variable {
  lazy val domain = List(1.0, 0.0)
}

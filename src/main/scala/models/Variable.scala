package org.dennybritz.sampler

case class VariableValue(val value: Double) extends AnyVal

sealed trait Variable {
  def id: Int
  def value: Double
  def isEvidence: Boolean
  def isQuery: Boolean
  def domain: List[Double]
  def factorIds: List[Int]
}

case class BooleanVariable(id: Int, value: Double, isEvidence: Boolean, isQuery: Boolean, factorIds: List[Int]) 
  extends Variable {
  lazy val domain = List(1.0, 0.0)
}

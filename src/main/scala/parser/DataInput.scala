package org.dennybritz.sampler

import scala.collection.immutable.HashMap

case class DataInput(
  weights: Vector[Weight], 
  variables: Vector[_ <: Variable], 
  factors: Vector[Factor])
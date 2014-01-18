package org.dennybritz.sampler

import scala.collection.immutable.HashMap

case class DataInput(
  weightsMap: Map[Int, Weight], 
  variablesMap: Map[Int, Variable], 
  factorsMap: Map[Int, Factor])
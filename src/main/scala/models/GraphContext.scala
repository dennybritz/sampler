package org.dennybritz.sampler

import scala.collection.JavaConversions._
import scala.collection.immutable.IntMap
import scala.collection.mutable.{HashMap => MHashMap, MultiMap, Set => MSet, ArrayBuffer}
import java.util.concurrent.ConcurrentHashMap


class GraphContext(
  val factors: Vector[Factor], val variables: Vector[_ <: Variable], val weights: Vector[Weight],
  val variableValues: ArrayBuffer[Double], val weightValues: ArrayBuffer[Double]) {

  def factorsForVariable(id: Int) = variables(id).factorIds

  def getVariableValue(id: Int, isPositive : Boolean = true) : Double = {
    if (isPositive) variableValues(id) else 1.0 - variableValues(id)
  }

  def getWeightValue(id: Int) = weightValues(id)

  def updateVariableValues(newValues: Map[Int, Double]) = {
    newValues.par foreach { case(key, value) => variableValues(key) = value }
  }


  def updateVariableValue(variableId: Int, newValue: Double) = {
    variableValues(variableId) = newValue
  }

  def updateWeightValue(weightId: Int, newValue: Double) = {
    weightValues(weightId) = newValue
  }

}

object GraphContext {
  def create(input: DataInput) : GraphContext = {

    // Assert that we have sequential ids for indexing
    input.factors.iterator.map(_.id).zipWithIndex.find(t => t._1 != t._2).foreach { case(idx, factorId) =>
      throw new IllegalArgumentException(s"Factor ${factorId} did not match id=${idx}")
    }
    input.variables.iterator.map(_.id).zipWithIndex.find(t => t._1 != t._2).foreach { case(idx, variableId) =>
      throw new IllegalArgumentException(s"Variable ${variableId} did not match id=${idx}")
    }
    input.weights.iterator.map(_.id).zipWithIndex.find(t => t._1 != t._2).foreach { case(idx, weightId) =>
      throw new IllegalArgumentException(s"Weight ${weightId} did not match id=${idx}")
    }

    // val variableFactorMap = tmpVariableFactorMap.toMap.mapValues(_.toSet)
    val variableValues = ArrayBuffer(input.variables.map(_.value): _*)
    val weightValues = ArrayBuffer(input.weights.map(_.value): _*)

    new GraphContext(input.factors, input.variables, input.weights, variableValues, weightValues)
  }
}
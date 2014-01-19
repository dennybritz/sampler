package org.dennybritz.sampler

import scala.collection.JavaConversions._
import scala.collection.immutable.IntMap
import scala.collection.mutable.{HashMap => MHashMap, MultiMap, Set => MSet}
import java.util.concurrent.ConcurrentHashMap


class GraphContext(
  _factosMap: IntMap[Factor], _variablesMap: IntMap[Variable], _weightsMap: IntMap[Weight],
  _variableFactorMap: IntMap[Set[Int]], 
  _variableValues: ConcurrentHashMap[Int, Double], _weightValues: ConcurrentHashMap[Int, Double]) {

  def factorsMap = _factosMap
  def variablesMap = _variablesMap
  def weightsMap = _weightsMap
  def variableFactorMap = _variableFactorMap
  def variableValues = _variableValues
  def weightValues = _weightValues

  def getVariableValue(id: Int) = _variableValues(id)
  def getWeightValue(id: Int) = _weightValues(id)

  def updateVariableValues(newValues: Map[Int, Double]) = {
    newValues foreach { case(key, value) => variableValues.update(key, value) }
  }

  def updateWeightValues(newValues: Map[Int, Double]) = {
    newValues foreach { case(key, value) => weightValues.update(key, value) }
  }

  def updateVariableValue(variableId: Int, newValue: Double) = 
    _variableValues.update(variableId, newValue)

  def updateWeightValue(weightId: Int, newValue: Double) = 
    _weightValues.update(weightId, newValue)

}

object GraphContext {
  def create(input: DataInput) : GraphContext = {

    // TOOD: Can we make this functional?
    val tmpVariableFactorMap = new MHashMap[Int, MSet[Int]] with MultiMap[Int, Int]
    input.factorsMap.foreach { case(factorId, factor) =>
      factor.variables.foreach( v => tmpVariableFactorMap.addBinding(v.id, factorId))
    }
    // Add empty binding for all variables that don't have factors
    input.variablesMap.keys.foreach { variableId =>
      if (!tmpVariableFactorMap.contains(variableId)) {
        tmpVariableFactorMap.put(variableId, MSet[Int]())
      }
    }

    val variableFactorMap = tmpVariableFactorMap.toMap.mapValues(_.toSet)
    val variableValues = new ConcurrentHashMap[Int, Double](input.variablesMap.mapValues(_.value))
    val weightValues = new ConcurrentHashMap[Int, Double](input.weightsMap.mapValues(_.value))

    new GraphContext(IntMap(input.factorsMap.toSeq: _*), 
      IntMap(input.variablesMap.toSeq: _*), IntMap(input.weightsMap.toSeq: _*),
      IntMap(variableFactorMap.toSeq: _*), variableValues, weightValues)
  }
}
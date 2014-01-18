package org.dennybritz.sampler

import scala.collection.immutable.HashMap
import scala.collection.mutable.{HashMap => MHashMap, MultiMap, Set => MSet}


class GraphContext(objects: DataInput, _variableFactorMap: Map[Int, Set[Int]], 
  _variableValues: MHashMap[Int, Double], _weightValues: MHashMap[Int, Double]) {

  def factorsMap = objects.factorsMap
  def variablesMap = objects.variablesMap
  def weightsMap = objects.weightsMap
  def variableFactorMap = _variableFactorMap
  def variableValues = _variableValues
  def weightValues = _weightValues

  def updateVariableValues(newValues: Map[Int, Double]) = {
    newValues foreach { case(key, value) => variableValues.update(key, value) }
  }

  def updateWeightValues(newValues: Map[Int, Double]) = {
    newValues foreach { case(key, value) => weightValues.update(key, value) }
  }

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
    val variableValues = MHashMap[Int, Double](input.variablesMap.mapValues(_.value).toSeq: _*)
    val weightValues = MHashMap[Int, Double](input.weightsMap.mapValues(_.value).toSeq: _*)

    new GraphContext(input, variableFactorMap, variableValues, weightValues)
  }
}
package org.dennybritz.sampler

import scala.util.Random
import scala.collection.immutable.IntMap
import scala.concurrent._
import scala.concurrent.duration._

object SamplingUtils extends Logging {

  /* Evaluates a factor given the values in the current context */
  def evaluateFactor(factorId: Int)(implicit context: GraphContext) : Double = {
    val factor = context.factors(factorId)
    val factorVariables = factor.variables.map(v => context.getVariableValue(v.id, v.isPositive))
    factor.function.evaluate(factorVariables)
  }

  /* Samples a variable and updates its value in the context */
  def sampleVariable(variableId: Int)(implicit context: GraphContext) : Unit  = {
    // All factors that connect to the variable
    val variableFactors = context.factorsForVariable(variableId).map(context.factors.apply)

    // TODO: Be domain-independent
    val (positiveSum, negativeSum) = variableFactors.toList.map { factor =>
      val factorWeightValue = context.getWeightValue(factor.weightId)
      val (positiveCase, negativeCase) = factor.variables.collect {
        case FactorVariable(`variableId`, true, _) => (1.0, 0.0)
        case FactorVariable(`variableId`, false, _) => (0.0, 1.0)
        case FactorVariable(someVariableId, isPositive, _) => 
          val variableValue = context.getVariableValue(someVariableId, isPositive)
          (variableValue, variableValue)
      }.unzip
      (factor.function.evaluate(positiveCase) * factorWeightValue, 
        factor.function.evaluate(negativeCase) * factorWeightValue)
    }.reduce { (x,y) => (x._1 + y._1, x._2 + y._2) }

    val newValue = if ((Random.nextDouble * (1.0 + math.exp(negativeSum - positiveSum))) <= 1.0) 1.0 else 0.0
    context.updateVariableValue(variableId, newValue)
  }

  /* Samples multiple variables and updates the variable values in the context */
  def sampleVariables(variables: Set[Int])(implicit context: GraphContext) : Unit = {
    variables.par.foreach(sampleVariable)
  }

}
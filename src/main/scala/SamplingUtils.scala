package org.dennybritz.sampler

import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration._

object SamplingUtils extends Logging {

  // Use constant parallelism based on the number of cores
  lazy val _parallelism = Runtime.getRuntime.availableProcessors.toInt
  import ExecutionContext.Implicits.global

  def parallelism = _parallelism

  /* Samples a variable and updates its value in the context */
  def sampleVariable(variableId: Int)(implicit context: GraphContext) : Unit  = {
    // All factors that connect to the variable
    val variableFactors = context.variableFactorMap(variableId) map (context.factorsMap.apply)

    // TODO: Be domain-independent
    val (positiveValues, negativeValues) = variableFactors.toList.map { factor =>
      val factorWeightValue = context.getWeightValue(factor.weightId)
      val (positiveCase, negativeCase) = factor.variables.collect {
        case FactorVariable(`variableId`, true) => (1.0, 0.0)
        case FactorVariable(`variableId`, false) => (0.0, 1.0)
        case FactorVariable(someVariableId, isPositive) => 
          val variableValue = context.getVariableValue(someVariableId, isPositive)
          (variableValue, variableValue)
      }.unzip

      (factor.function.evaluate(positiveCase) * factorWeightValue, 
        factor.function.evaluate(negativeCase) * factorWeightValue)
    }.unzip

    val positiveSum = positiveValues.sum
    val negativeSum = negativeValues.sum

    // TODO: ?
    val newValue = if ((Random.nextDouble * (1.0 + math.exp(negativeSum - positiveSum))) <= 1.0) 1.0 else 0.0
    context.updateVariableValue(variableId, newValue)
  }

  /* Samples multiple variables and updates the variable values in the context */
  def sampleVariables(variables: Set[Int])(implicit context: GraphContext) : Unit = {
    val groupSize = Math.max((variables.size / SamplingUtils.parallelism).toInt, 1)
    val partitionedVariables = variables.iterator.grouped(groupSize)
    val tasks = partitionedVariables.map { variables =>
      future { Random.shuffle(variables).foreach(sampleVariable) }
    }
    val mergedResults = Future.sequence(tasks)
    Await.result(mergedResults, 1337.hours)
  }

}
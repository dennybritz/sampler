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
    val (positiveValues, negativeValues) = variableFactors.map { factor =>
      val factorWeightValue = context.getWeightValue(factor.weightId)
      val variableIndex = factor.variables.map(_.id).indexOf(variableId)
      val variableValues = factor.variables.map(_.id).map(context.getVariableValue)
      val (positiveValue, negativeValue) = if (factor.variables(variableIndex).isPositive) (1.0, 0.0) else (0.0, 1.0)
      val positiveCase = variableValues.updated(variableIndex, positiveValue)
      val negativeCase = variableValues.updated(variableIndex, negativeValue)
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
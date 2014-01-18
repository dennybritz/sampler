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
  def sampleVariable(variable: Variable)(implicit context: GraphContext) : Unit  = {
    // All factors that connect to the variable
    val variableFactors = context.variableFactorMap(variable.id) map (context.factorsMap.apply)

    // TODO: Be domain-independent
    val (positiveValues, negativeValues) = variableFactors.map { factor =>
      val factorWeightValue = context.weightValues(factor.weightId)
      val variableIndex = factor.variables.map(_.id).indexOf(variable.id)
      val variableValues = factor.variables.map(_.id).map(context.variableValues.get)
      val positiveCase = variableValues.updated(variableIndex, 1.0)
      val negativeCase = variableValues.updated(variableIndex, 0.0)
      (factor.function.evaluate(positiveCase) * factorWeightValue, 
        factor.function.evaluate(negativeCase) * factorWeightValue)
    }.unzip

    val positiveSum = positiveValues.sum
    val negativeSum = negativeValues.sum

    // TODO: ?
    val newValue = if ((Random.nextDouble * (1.0 + math.exp(negativeSum - positiveSum))) <= 1.0) 1.0 else 0.0
    context.updateVariableValue(variable.id, newValue)
  }

  /* Samples multiple variables and returns the sampled values */
  def sampleVariables(variables: Set[Variable])(implicit context: GraphContext) : Unit = {
    val groupSize = Math.max((variables.size / SamplingUtils.parallelism).toInt, 1)
    val partitionedVariables = Random.shuffle(variables).grouped(groupSize)
    val tasks = partitionedVariables.map { variables =>
      future { variables.foreach(v => sampleVariable(v)) }
    }
    val mergedResults = Future.sequence(tasks)
    Await.result(mergedResults, 1337.hours)
  }

  
  

}
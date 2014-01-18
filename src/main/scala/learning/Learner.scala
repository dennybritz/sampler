package org.dennybritz.sampler

import scala.collection.mutable.{HashMap => MHashMap}
import scala.collection.immutable.HashMap

class Learner(context: GraphContext) extends Logging {

  // Make the context implicit
  implicit val _context = context

  // Computes the marginal probability that each factor is true
  // TODO: How to generalize this?
  def sampleFactors(variables: Iterable[Variable], factors: Iterable[Factor], 
    numSamples: Int) : Map[Int, Double] = {
    (0 until numSamples).foldLeft(Map.empty[Int, Double]) { case(values, iteration) =>
      // Sample all variables and update their values in the context
      val samplingResults = SamplingUtils.sampleVariables(variables)
      context.updateVariableValues(samplingResults)
      // Evaluate each factor and generate a Map from FactorID -> Value
      factors.map { factor =>
        val factorVariables = factor.variables map (fv => context.variableValues(fv.id))
        val factorValue = factor.function.evaluate(factorVariables)
        (factor.id, values.get(factor.id).getOrElse(0.0) + factorValue)
      }.toMap
    }.mapValues(_ / numSamples) 
  }


  def learnWeights(numIterations: Int, numSamples: Int, learningRate: Double, regularizationConstant: Double,
    diminishRate: Double) = {

    log.debug(s"learning weights")
    val (queryVariables, evidenceVariables) = context.variablesMap.values.toSet partition (_.isQuery)
    val evidenceValues = evidenceVariables.map(v => (v.id, v.value)).toMap
    // We only learn weights for factors that are connected to evidence
    val evidenceFactorIds = evidenceVariables.map(_.id).flatMap(context.variableFactorMap(_)).toSet
    val queryFactors = context.factorsMap.filterKeys(evidenceFactorIds).values.toSet
    val queryWeightIds = queryFactors.map(_.weightId).toSet
    val queryWeights = context.weightsMap.filterKeys(queryWeightIds)

    log.debug(s"num_factors=${context.factorsMap.size} num_query_factors=${queryFactors.size}")
    log.debug(s"num_weights=${context.weightsMap.size} num_query_weights=${queryWeights.size}")
    log.debug(s"num_query_variables=${queryVariables.size} num_evidence_variables=${evidenceVariables.size}")

    for(i <- 0 until numIterations) {

      val iterLearningRate = math.pow(diminishRate, i) * learningRate
      
      log.debug(s"iteration=${i} learning_rate=${iterLearningRate}")

      // Compute the expectation for all factors sampling only query variables
      val conditionedEx = sampleFactors(queryVariables, queryFactors, numSamples)

      // Compute the expectation for all factors sampling all variables
      val unconditionedEx = sampleFactors(queryVariables ++ evidenceVariables, queryFactors, numSamples)

      // Compute the weight changes
      val weightChanges = queryFactors.groupBy(_.weightId).mapValues { factors =>
        val factorWeightChanges = factors.toList map (f => (conditionedEx(f.id) - unconditionedEx(f.id)))
        factorWeightChanges.sum
      }

      // Apply the weight changes using the learning rate
      val newWeights = weightChanges.map { case(weightId, delta) =>
        val currentWeight = context.weightValues(weightId)
        val newWeight = currentWeight+ (delta * iterLearningRate) * (1.0/(1.0+regularizationConstant*iterLearningRate))
        (weightId, newWeight)
      }
      context.updateWeightValues(newWeights)

      // Calculate the L2 norm of the weight changes and the maximum gradient
      val gradientNorm = math.sqrt(weightChanges.values.map(math.pow(_, 2)).sum)
      val maxGradient = weightChanges.values.maxBy(Math.abs)
      log.debug(s"gradient_norm=${gradientNorm} max_gradient=${maxGradient}")

      // Reset the evidence variables to their evidence values 
      // (we changed their values by sampling them above)
      context.updateVariableValues(evidenceValues)

    }

    // Return a map of weightId -> weightValue
    context.weightValues.toMap
    
  }


}
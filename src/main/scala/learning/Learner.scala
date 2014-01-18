package org.dennybritz.sampler

import scala.collection.mutable.{HashMap => MHashMap}
import scala.collection.immutable.HashMap

class Learner(context: GraphContext) extends Logging {

  // Make the context implicit
  implicit val _context = context

  // Computes the marginal probability that each factor is true
  // TODO: How to generalize this?
  def sampleFactors(variables: Set[Int], factors: Set[Int], 
    numSamples: Int) : Map[Int, Double] = {
    (0 until numSamples).foldLeft(Map.empty[Int, Double]) { case(values, iteration) =>
      // Sample all variables and update their values in the context
      SamplingUtils.sampleVariables(variables)
      // Evaluate each factor and generate a Map from FactorID -> Value
      factors.map { factorId =>
        val factor = context.factorsMap(factorId)
        val factorVariables = factor.variables map (fv => context.getVariableValue(fv.id))
        val factorValue = factor.function.evaluate(factorVariables)
        (factorId, values.get(factorId).getOrElse(0.0) + factorValue)
      }.toMap
    }.mapValues(_ / numSamples) 
  }


  def learnWeights(numIterations: Int, numSamples: Int, learningRate: Double, regularizationConstant: Double,
    diminishRate: Double) = {

    log.debug(s"learning weights")

    val queryVariables = context.variablesMap.values.filter(_.isQuery).map(_.id).toSet
    val evidenceVariables = context.variablesMap.values.filter(_.isEvidence).map(_.id).toSet
    val evidenceValues = context.variablesMap.filterKeys(evidenceVariables.contains).mapValues(_.value)
    // We only learn weights for factors that are connected to evidence
    val evidenceFactorIds = evidenceVariables.flatMap(context.variableFactorMap(_)).toSet
    val queryFactors = context.factorsMap.filterKeys(evidenceFactorIds).values.map(_.id).toSet
    val queryWeightIds = context.factorsMap.filterKeys(evidenceFactorIds).values.map(_.weightId).toSet
    // Map from weight -> Factors
    val weightFactorMap = context.factorsMap.filterKeys(evidenceFactorIds).values.groupBy(_.weightId)
      .mapValues(_.map(_.id))

    log.debug(s"num_factors=${context.factorsMap.size} num_query_factors=${queryFactors.size}")
    log.debug(s"num_weights=${context.weightsMap.size} num_query_weights=${queryWeightIds.size}")
    log.debug(s"num_query_variables=${queryVariables.size} num_evidence_variables=${evidenceVariables.size}")

    for(i <- 0 until numIterations) {

      val iterLearningRate = math.pow(diminishRate, i) * learningRate
      
      log.debug(s"iteration=${i} learning_rate=${iterLearningRate}")

      // Compute the expectation for all factors sampling only query variables
      val conditionedEx = sampleFactors(queryVariables, queryFactors, numSamples)

      // Compute the expectation for all factors sampling all variables
      val unconditionedEx = sampleFactors(queryVariables ++ evidenceVariables, queryFactors, numSamples)

      // Apply the weight changes
      val weightChanges = queryWeightIds.map { weightId =>
        val factors = weightFactorMap(weightId)
        val currentWeight = context.weightValues(weightId)
        val weightChange = factors map (f => (conditionedEx(f) - unconditionedEx(f))) sum
        val newWeight = currentWeight + (weightChange * iterLearningRate) * (1.0/(1.0+regularizationConstant*iterLearningRate))
        context.updateWeightValue(weightId, newWeight)
        weightChange
      }

      // Calculate the L2 norm of the weight changes and the maximum gradient
      val gradientNorm = math.sqrt(weightChanges.map(math.pow(_, 2)).sum)
      val maxGradient = weightChanges.maxBy(Math.abs)
      log.debug(s"gradient_norm=${gradientNorm} max_gradient=${maxGradient}")

      // Reset the evidence variables to their evidence values 
      // (we changed their values by sampling them above)
      context.updateVariableValues(evidenceValues)

    }

    // Return a map of weightId -> weightValue
    context.weightValues.toMap
    
  }


}
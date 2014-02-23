package org.dennybritz.sampler

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap => MHashMap}
import scala.collection.immutable.HashMap
import scala.concurrent._
import scala.concurrent.duration._

class Learner(context: GraphContext) extends Logging {

  // Make the context implicit
  implicit val _context = context

  def learnWeights(numIterations: Int, numSamples: Int, learningRate: Double, regularizationConstant: Double,
    diminishRate: Double) : Array[Double] = {

    val queryVariableIds = context.variables.filter(_.isQuery).map(_.id).toSet
    val evidenceVariableIds =  context.variables.filter(!_.isQuery).map(_.id).toSet
    val evidenceValueMap = evidenceVariableIds.map(id => (id, context.getVariableValue(id))).toMap
    
    // We only learn weights for factors that are connected to evidence
    val queryFactorIds = evidenceVariableIds.flatMap(context.factorsForVariable(_)).toSet
    val queryWeightIds = queryFactorIds.map(context.factors.apply)
      .map(f => context.weights(f.weightId)).filterNot(_.isFixed).map(_.id)
    val weightFactorMap = queryFactorIds.map(context.factors.apply).groupBy(_.weightId).mapValues(_.map(_.id))

    log.debug(s"num_iterations=${numIterations}")
    log.debug(s"num_samples_per_iteration=${numSamples}")
    log.debug(s"learning_rate=${learningRate}")
    log.debug(s"diminish_rate=${diminishRate}")
    log.debug(s"regularization_constant=${regularizationConstant}")
    log.debug(s"num_factors=${context.factors.size} num_query_factors=${queryFactorIds.size}")
    log.debug(s"num_weights=${context.weights.size} num_query_weights=${queryWeightIds.size}")
    log.debug(s"num_query_variables=${queryVariableIds.size} num_evidence_variables=${evidenceVariableIds.size}")

    if(queryWeightIds.size == 0) {
      log.debug("no query weights, nothing to learn!")
      return context.weightValues.toArray
    }

    // Initial values for factors
    val initialValues = queryFactorIds.map(id => (id, 0.0)).toMap

    for(i <- 0 until numIterations) {

      val iterLearningRate = math.pow(diminishRate, i) * learningRate
      
      log.debug(s"iteration=${i} learning_rate=${iterLearningRate}")

      val conditionedEx = (0 until numSamples).foldLeft(initialValues) { case(currentValues, iteration) =>
        SamplingUtils.sampleVariables(queryVariableIds)
        // val factorValues = SamplingUtils.evaluateFactors(queryFactorIds)
        currentValues.par.map { case(k,v) =>
          (k, v + SamplingUtils.evaluateFactor(k))
        }.toMap.seq
      }.par.mapValues(_ / numSamples) 
     
      // Compute the expectation for all factors sampling all variables
      val unconditionedEx = (0 until numSamples).foldLeft(initialValues) { case(currentValues, iteration) =>
        SamplingUtils.sampleVariables(queryVariableIds ++ evidenceVariableIds)
        currentValues.par.map { case(k,v) =>
          (k, v + SamplingUtils.evaluateFactor(k))
        }.toMap.seq
      }.par.mapValues(_ / numSamples) 

      // Apply the weight changes in parallel
      val weightChanges = queryWeightIds.par.map { weightId =>
        val factors = weightFactorMap(weightId)
        val weightChange = factors.foldLeft(0.0) { case(x, f) => x + (conditionedEx(f) - unconditionedEx(f)) }
        (weightId, weightChange)
      }.toMap

      weightChanges.par.foreach { case(weightId, weightChange) => 
        val currentWeight = context.getWeightValue(weightId)
        val newWeight = currentWeight + (weightChange * iterLearningRate) * 
          (1.0/(1.0+regularizationConstant*iterLearningRate))
        context.updateWeightValue(weightId, newWeight)
      }

      // Calculate the L2 norm of the weight changes and the maximum gradient
      val gradientNorm = math.sqrt(weightChanges.par.values.map(math.pow(_, 2)).sum)
      val maxGradient = weightChanges.par.values.maxBy(Math.abs)
      log.debug(s"gradient_norm=${gradientNorm} max_gradient=${maxGradient}")

      // Reset the evidence variables to their evidence values 
      // (we changed their values by sampling them above)
      context.updateVariableValues(evidenceValueMap)
    }

    // Return a map of weightId -> weightValue
    context.weightValues.toArray
    
  }


}
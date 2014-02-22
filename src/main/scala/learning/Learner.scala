package org.dennybritz.sampler

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap => MHashMap}
import scala.collection.immutable.HashMap
import scala.concurrent._
import scala.concurrent.duration._

class Learner(context: GraphContext) extends Logging {

  // Make the context implicit
  implicit val _context = context

  def evaluateFactor(factorId: Int) : Double = {
    val factor = context.factors(factorId)
    val factorVariables = factor.variables map (fv => context.getVariableValue(fv.id))
    factor.function.evaluate(factorVariables)
  }

  // Computes the marginal probability that each factor is true
  // TODO: How to generalize this?
  def sampleFactors(variables: Set[Int], factors: Set[Int], 
    numSamples: Int) : Map[Int, Double] = {

    val defaultMap = Map.empty[Int, Double].withDefaultValue(0.0)
    (0 until numSamples).foldLeft(defaultMap) { case(values, iteration) =>
      // Sample all variables and update their values in the context
      SamplingUtils.sampleVariables(variables)
      // Evaluate all factors
      factors.par.map { factorId => (factorId, evaluateFactor(factorId) + values(factorId)) }.toMap.seq
    }.mapValues(_ / numSamples) 
  }


  def learnWeights(numIterations: Int, numSamples: Int, learningRate: Double, regularizationConstant: Double,
    diminishRate: Double) : Array[Double] = {

    val queryVariables = context.variables.filter(_.isQuery).map(_.id).toSet
    val evidenceVariables =  context.variables.filter(!_.isQuery).map(_.id).toSet
    val evidenceValues = evidenceVariables.map(id => (id, context.getVariableValue(id))).toMap
    // We only learn weights for factors that are connected to evidence
    val queryFactors = evidenceVariables.flatMap(context.factorsForVariable(_)).toSet
    val factorWeightIds = context.factors.filter(x => queryFactors.contains(x.id)).map(_.weightId).toSet
    val queryWeightIds = for {
      weightId <- factorWeightIds
      weight = context.weights(weightId)
      if !weight.isFixed
    } yield weight.id
    // Map from weight -> Factors
    val weightFactorMap = queryFactors.map(context.factors.apply).groupBy(_.weightId).mapValues(_.map(_.id))

    log.debug(s"num_iterations=${numIterations}")
    log.debug(s"num_samples_per_iteration=${numSamples}")
    log.debug(s"learning_rate=${learningRate}")
    log.debug(s"diminish_rate=${diminishRate}")
    log.debug(s"regularization_constant=${regularizationConstant}")
    log.debug(s"num_factors=${context.factors.size} num_query_factors=${queryFactors.size}")
    log.debug(s"num_weights=${context.weights.size} num_query_weights=${queryWeightIds.size}")
    log.debug(s"num_query_variables=${queryVariables.size} num_evidence_variables=${evidenceVariables.size}")

    if(queryWeightIds.size == 0) {
      log.debug("no query weights, nothing to learn!")
      return Array()
    }

    for(i <- 0 until numIterations) {

      val iterLearningRate = math.pow(diminishRate, i) * learningRate
      
      log.debug(s"iteration=${i} learning_rate=${iterLearningRate}")

      // Compute the expectation for all factors sampling only query variables
      val conditionedEx = sampleFactors(queryVariables, queryFactors, numSamples)

      // Compute the expectation for all factors sampling all variables
      val unconditionedEx = sampleFactors(queryVariables ++ evidenceVariables, queryFactors, numSamples)

      // Apply the weight changes in parallel
      val weightChanges = queryWeightIds.par.map { weightId =>
        val factors = weightFactorMap(weightId)
        val currentWeight = context.getWeightValue(weightId)
        val weightChange = factors.foldLeft(0.0) { case(x, f) => x + (conditionedEx(f) - unconditionedEx(f)) }
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
    context.weightValues.toArray
    
  }


}
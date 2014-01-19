package org.dennybritz.sampler

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap => MHashMap}
import scala.collection.immutable.HashMap
import scala.concurrent._
import scala.concurrent.duration._

class Learner(context: GraphContext) extends Logging {

  // Make the context implicit
  implicit val _context = context
  import ExecutionContext.Implicits.global


  def evaluateFactor(factorId: Int) : Double = {
    val factor = context.factorsMap(factorId)
    val factorVariables = factor.variables map (fv => context.getVariableValue(fv.id))
    factor.function.evaluate(factorVariables)
  }

  // Computes the marginal probability that each factor is true
  // TODO: How to generalize this?
  def sampleFactors(variables: Set[Int], factors: Set[Int], 
    numSamples: Int) : Map[Int, Double] = {
    
    // Partition size for parallelizing factor evaluation
    val partitionSize = Math.max((factors.size / SamplingUtils.parallelism).toInt, 1)
   
    val defaultMap = Map.empty[Int, Double].withDefaultValue(0.0)
    (0 until numSamples).foldLeft(defaultMap) { case(values, iteration) =>
      // Sample all variables and update their values in the context
      SamplingUtils.sampleVariables(variables)
      
      // Evaluate all factors
      val tasks = factors.iterator.grouped(partitionSize).map { factors =>
        Future { 
          factors.map { factorId => (factorId, evaluateFactor(factorId) + values(factorId)) }.toMap
        }.mapTo[Map[Int, Double]]
      }
      val mergedResults = Future.sequence(tasks)
      Await.result(mergedResults, 1337.hours).reduce(_ ++ _)
    }.mapValues(_ / numSamples) 
  }


  def learnWeights(numIterations: Int, numSamples: Int, learningRate: Double, regularizationConstant: Double,
    diminishRate: Double) : Map[Int, Double] = {

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
    val weightPartitionSize = Math.max((queryWeightIds.size / SamplingUtils.parallelism).toInt, 1)

    log.debug(s"num_iterations=${numIterations}")
    log.debug(s"num_samples_per_iteration=${numSamples}")
    log.debug(s"learning_rate=${learningRate}")
    log.debug(s"diminish_rate=${diminishRate}")
    log.debug(s"regularization_constant=${regularizationConstant}")
    log.debug(s"num_factors=${context.factorsMap.size} num_query_factors=${queryFactors.size}")
    log.debug(s"num_weights=${context.weightsMap.size} num_query_weights=${queryWeightIds.size}")
    log.debug(s"num_query_variables=${queryVariables.size} num_evidence_variables=${evidenceVariables.size}")

    if(queryWeightIds.size == 0) {
      log.debug("no query weights, nothing to learn!")
      return Map.empty[Int, Double]
    }

    for(i <- 0 until numIterations) {

      val iterLearningRate = math.pow(diminishRate, i) * learningRate
      
      log.debug(s"iteration=${i} learning_rate=${iterLearningRate}")

      // Compute the expectation for all factors sampling only query variables
      val conditionedEx = sampleFactors(queryVariables, queryFactors, numSamples)

      // Compute the expectation for all factors sampling all variables
      val unconditionedEx = sampleFactors(queryVariables ++ evidenceVariables, queryFactors, numSamples)

      // Apply the weight changes in parallel

      val tasks = queryWeightIds.iterator.grouped(weightPartitionSize).map { weightBatch =>
        Future {
          weightBatch.map { weightId =>
            val factors = weightFactorMap(weightId)
            val currentWeight = context.getWeightValue(weightId)
            val weightChange = factors.foldLeft(0.0) { case(x, f) => x + (conditionedEx(f) - unconditionedEx(f)) }
            val newWeight = currentWeight + (weightChange * iterLearningRate) * (1.0/(1.0+regularizationConstant*iterLearningRate))
            context.updateWeightValue(weightId, newWeight)
            weightChange
          }
        }.mapTo[Seq[Double]]
      }
      val taskSequence = Future.sequence(tasks)
      val weightChanges = Await.result(taskSequence, 1337.hours).reduce(_ ++ _)

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
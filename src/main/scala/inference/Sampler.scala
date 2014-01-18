package org.dennybritz.sampler

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{MultiMap, HashMap, Set => MSet}
import scala.util.Random

class Sampler(context: GraphContext) extends Logging {

  // Make the context implicit
  implicit val _context = context

  // Sums of all samples values to calculate the expectation
  val sampleSums = HashMap[Int, Double]()
  // Squared sample sums to calculate running standard deviation
  val sampleSums2 = HashMap[Int, Double]()

  def calculateMarginals(numSamples: Int, variables: Seq[Variable]) : Map[Int, VariableInferenceResult] = {
    
    log.debug(s"calculating marginals for num_vars=${variables.size}")

    val nonEvidenceVariables = variables.filterNot(_.isEvidence).map(_.id).toSet

    // TODO: Z-test for convergence

    // For each iteration
    for (i <- 1 to numSamples) {
      log.debug(s"inference_iteration=${i}")
      // Samples all variables that are not evidence
      SamplingUtils.sampleVariables(nonEvidenceVariables)
      // Updated the sample sums
      nonEvidenceVariables.iterator.foreach { variableId =>
        val sampleResult = context.getVariableValue(variableId)
        sampleSums.put(variableId, sampleSums.get(variableId).getOrElse(0.0) + sampleResult)
        sampleSums2.put(variableId, sampleSums2.get(variableId).getOrElse(0.0) + math.pow(sampleResult, 2))
      }
    }

    // Return the inferenceResult
    nonEvidenceVariables.map { variableId =>
      val result = VariableInferenceResult(
        sampleSums(variableId) / numSamples.toDouble,
        math.sqrt(numSamples * sampleSums2(variableId) - math.pow(sampleSums(variableId), 2)) / numSamples,
        context.getVariableValue(variableId))
      (variableId, result)
    }.toMap
  }

}
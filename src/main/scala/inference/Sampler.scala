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

    val nonEvidenceVariables = variables.filterNot(_.isEvidence)

    // TODO: Z-test for convergence

    // For each iteration
    for (i <- 1 to numSamples) {
      log.debug(s"inference_iteration=${i}")
      // Samples all variables that are not evidence
      val sampleResults = SamplingUtils.sampleVariables(nonEvidenceVariables)
      context.updateVariableValues(sampleResults)
      // Updated the sample sums
      sampleResults.foreach { case(variableId, sampleResult) =>
        sampleSums.put(variableId, sampleSums.get(variableId).getOrElse(0.0) + sampleResult)
        sampleSums2.put(variableId, sampleSums2.get(variableId).getOrElse(0.0) + math.pow(sampleResult, 2))
      }
    }

    // Return the inferenceResult
    nonEvidenceVariables.map { variable =>
      val result = VariableInferenceResult(
        sampleSums(variable.id) / numSamples.toDouble,
        math.sqrt(numSamples * sampleSums2(variable.id) - math.pow(sampleSums(variable.id), 2)) / numSamples,
        context.variableValues(variable.id))
      (variable.id, result)
    }.toMap
  }

}
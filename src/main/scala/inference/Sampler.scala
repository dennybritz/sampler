package org.dennybritz.sampler

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{MultiMap, HashMap, Set => MSet, ArrayBuffer}
import scala.util.Random

class Sampler(context: GraphContext) extends Logging {

  // Make the context implicit
  implicit val _context = context


  def calculateMarginals(numSamples: Int, variables: Seq[Variable]) : InferenceResult = {
    
    log.debug(s"calculating marginals for num_vars=${variables.size}")

    val nonEvidenceVariables = variables.filterNot(_.isEvidence).map(_.id).toSet

    val iteration20 = (numSamples * 0.2).toInt
    val iteration50 = (numSamples * 0.5).toInt
    // Sums of all samples values to calculate the expectation
    val sampleSums = ArrayBuffer.fill[Double](variables.size) { 0.0 }
    // Squared sample sums to calculate running standard deviation
    val sampleSums2 =  ArrayBuffer.fill[Double](variables.size) { 0.0 }
    var sampleSumsFirst20 =  ArrayBuffer.fill[Double](variables.size) { 0.0 }
    var sampleSums2First20 =  ArrayBuffer.fill[Double](variables.size) { 0.0 }
    var sampleSumsLast50 =  ArrayBuffer.fill[Double](variables.size) { 0.0 }
    var sampleSums2Last50 =  ArrayBuffer.fill[Double](variables.size) { 0.0 }

    // For each iteration
    for (i <- 1 to numSamples) {
      
      log.debug(s"iteration=${i}/${numSamples}")
      // Samples all variables that are not evidence
      val startTime = System.currentTimeMillis 
      SamplingUtils.sampleVariables(nonEvidenceVariables)
      val endTime = System.currentTimeMillis 

      log.info(s"samples/sec=${(1000.0 * nonEvidenceVariables.size / (endTime - startTime)).toInt}")
      
      // Updated the significance statistics
      // We can do this in parallel because we are never accessing the same variable twice
      nonEvidenceVariables.par.foreach { variableId =>
        val sampleResult = context.getVariableValue(variableId)
        sampleSums(variableId) += sampleResult
        sampleSums2(variableId) += math.pow(sampleResult, 2)
        if (i <= iteration20) {
          sampleSumsFirst20(variableId) += sampleResult
          sampleSums2First20(variableId) += math.pow(sampleResult, 2)
        }
        if (i >= iteration50) {
          sampleSumsLast50(variableId) += sampleResult
          sampleSums2Last50(variableId) += math.pow(sampleResult, 2)
        }
      }
      
    }

    // Calculate significane statistics
    log.debug("Calculating significant statistics...")
    val Tuple2(notConverged95, notConverged90) = nonEvidenceVariables.map { variableId =>
      val meanFirst20 = sampleSumsFirst20(variableId) / iteration20.toDouble
      val varianceFirst20 = sampleSums2First20(variableId)/iteration20.toDouble - (meanFirst20 * meanFirst20)
      val meanLast50 = sampleSumsLast50(variableId) / iteration50.toDouble
      val varianceLast50 = sampleSums2Last50(variableId)/iteration20.toDouble - (meanLast50 * meanLast50)

      val varianceSum = Math.max(varianceFirst20 + varianceLast50, 0.00001)
      val tScore = (meanFirst20 - meanLast50)/(Math.sqrt(varianceSum))

      val notConverged95 = (Math.abs(tScore) > 1.96)
      val notConverged90 = (Math.abs(tScore) > 1.65)
      (notConverged95, notConverged90)
    }.unzip

    log.debug(s"Not converged for p=0.95: ${notConverged95.count(_ == true)}/${nonEvidenceVariables.size}")
    log.debug(s"Not converged for p=0.90: ${notConverged90.count(_ == true)}/${nonEvidenceVariables.size}")

    // Generate the inference results
    val variableInferenceResults = nonEvidenceVariables.map { variableId =>
      VariableInferenceResult(variableId,
        sampleSums(variableId) / numSamples.toDouble,
        math.sqrt(numSamples * sampleSums2(variableId) - math.pow(sampleSums(variableId), 2)) / numSamples,
        context.getVariableValue(variableId))
    }.toList

    InferenceResult(variableInferenceResults)
  }

}

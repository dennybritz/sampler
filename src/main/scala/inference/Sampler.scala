package org.dennybritz.sampler

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{MultiMap, HashMap, Set => MSet}
import scala.util.Random

class Sampler(context: GraphContext) extends Logging {

  // Make the context implicit
  implicit val _context = context


  def calculateMarginals(numSamples: Int, variables: Seq[Variable]) : InferenceResult = {
    
    log.debug(s"calculating marginals for num_vars=${variables.size}")

    val nonEvidenceVariables = variables.filterNot(_.isEvidence).map(_.id).toSet

    // Sums of all samples values to calculate the expectation
    val sampleSums = HashMap[Int, Double]().withDefaultValue(0.0)
    // Squared sample sums to calculate running standard deviation
    val sampleSums2 = HashMap[Int, Double]().withDefaultValue(0.0)
    // We keep track of the variable values for the first 20% and last 50% of iterations.
    // We use the data for a Z-Test
    val iteration20 = (numSamples * 0.2).toInt
    val iteration50 = (numSamples * 0.5).toInt
    val sampleSumsFirst20 = HashMap[Int, Double]().withDefaultValue(0.0)
    val sampleSumsLast50 = HashMap[Int, Double]().withDefaultValue(0.0)
    val sampleSums2First20 = HashMap[Int, Double]().withDefaultValue(0.0)
    val sampleSums2Last50 = HashMap[Int, Double]().withDefaultValue(0.0)

    // TODO: Z-test for convergence

    // For each iteration
    for (i <- 1 to numSamples) {
      
      log.debug(s"iteration=${i}/${numSamples}")
      // Samples all variables that are not evidence
      SamplingUtils.sampleVariables(nonEvidenceVariables)
      
      // Updated the significance statistics
      nonEvidenceVariables.iterator.foreach { variableId =>
        val sampleResult = context.getVariableValue(variableId)
        sampleSums.put(variableId, sampleSums(variableId) + sampleResult)
        sampleSums2.put(variableId, sampleSums2(variableId) + math.pow(sampleResult, 2))
        if (i <= iteration20) {
          sampleSumsFirst20.put(variableId, sampleSumsFirst20(variableId) + sampleResult) 
          sampleSums2First20.put(variableId, sampleSums2First20(variableId) + math.pow(sampleResult, 2)) 
        }
        if (i >= iteration50) {
          sampleSumsLast50.put(variableId, sampleSumsLast50(variableId) + sampleResult)  
          sampleSums2Last50.put(variableId, sampleSums2Last50(variableId) + math.pow(sampleResult, 2)) 
        }
      }
      
    }

    // Calculate significane statistics
    log.debug("Calculating significant statistics...")
    val Tuple2(notConverged95, notConverged90) = nonEvidenceVariables.map { variableId =>
      val meanFirst20 = sampleSumsFirst20(variableId) / iteration20.toDouble
      val varianceFirst20 = sampleSums2First20(variableId) - (meanFirst20 * meanFirst20)
      val meanLast50 = sampleSumsLast50(variableId) / iteration50.toDouble
      val varianceLast50 = sampleSums2Last50(variableId) - (meanLast50 * meanLast50)

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


// 20 = sum_first_20(v)/NEPOCH_FIRST_20
//         mean_last_50 = sum_last_50(v)/NEPOCH_LATS_50
//         var_first_20 = mean_first_20 - mean_first_20*mean_first_20
//         var_last_50 =  mean_last_50 - mean_last_50*mean_last_50

//         if(var_first_20 + var_last_50 == 0){
//           t= (mean_first_20 - mean_last_50) * 10000
//         }else{
//           t = (mean_first_20 - mean_last_50)/(Math.sqrt(var_first_20+var_last_50))
//         }

//         if (t > 1.96 || t < -1.96){
//           rej_0_95 = rej_0_95 + 1
//         }


//         if (t > 1.65 || t < -1.65 ){
//           rej_0_90 = rej_0_90 + 1
//         }

//         nvariable = nvariable + 1
//         //println(t)
//       }

// var mean_first_20 = 0.0
//     var var_first_20 = 0.0
//     var mean_last_50 = 0.0
//     var var_last_50 = 0.0
//     var t = 0.0

//     var rej_0_95 = 0
//     var rej_0_90 = 0
//     var nvariable = 0

//     for(v <- variables){
//       if(!v.isevid){
//         mean_first_20 = sum_first_20(v)/NEPOCH_FIRST_20
//         mean_last_50 = sum_last_50(v)/NEPOCH_LATS_50
//         var_first_20 = mean_first_20 - mean_first_20*mean_first_20
//         var_last_50 =  mean_last_50 - mean_last_50*mean_last_50

//         if(var_first_20 + var_last_50 == 0){
//           t= (mean_first_20 - mean_last_50) * 10000
//         }else{
//           t = (mean_first_20 - mean_last_50)/(Math.sqrt(var_first_20+var_last_50))
//         }

//         if (t > 1.96 || t < -1.96){
//           rej_0_95 = rej_0_95 + 1
//         }


//         if (t > 1.65 || t < -1.65 ){
//           rej_0_90 = rej_0_90 + 1
//         }

//         nvariable = nvariable + 1
//         //println(t)
//       }
//     }

  // println("[INFERENCE INFO] " + "# NOT_CONVERGE (p=0.95) " + rej_0_95 + "/" + nvariable)
   // println("[INFERENCE INFO] " + "# NOT_CONVERGE (p=0.90) " + rej_0_90 + "/" + nvariable)

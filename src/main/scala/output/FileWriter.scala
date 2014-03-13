package org.dennybritz.sampler

import java.io.{File, FileOutputStream}
import org.deepdive.serialization.InferenceResultProtos

object FileWriter {

  def dumpVariables(variables: List[VariableInferenceResult], filename: String) {
    val file = new FileOutputStream(filename)
    variables.par.foreach { v =>
      val builder = InferenceResultProtos.VariableInferenceResult.newBuilder
      builder.setId(v.id)
      builder.setExpectation(v.expectation)
      builder.setId(v.id)
      val obj = builder.build()
      file.synchronized { obj.writeDelimitedTo(file) } 
    }
    file.close()
  }

  def dumpWeights(weights: Seq[Double], filename: String) = {
    val file = new FileOutputStream(filename)
    weights.zipWithIndex.par.foreach { case(value, weightId) =>
      val builder = InferenceResultProtos.WeightInferenceResult.newBuilder
      builder.setId(weightId)
      builder.setValue(value)
      val obj = builder.build()
      file.synchronized { obj.writeDelimitedTo(file) } 
    }
    file.close()
  }

}
package org.dennybritz.sampler

import java.io.File

object FileWriter {

  def dumpVariables(variables: List[VariableInferenceResult], filename: String) {
    val pw = new java.io.PrintWriter(new File(filename))
    variables.iterator.foreach { row =>
      pw.write(s"${row.id}\t${row.lastSample.toInt}\t${row.expectation}\n")
    }
    pw.close()
  }

  def dumpWeights(weights: Seq[Double], filename: String) = {
    val pw = new java.io.PrintWriter(new File(filename))
    weights.zipWithIndex.iterator.foreach { case(weightId, value) =>
      pw.write(s"${weightId}\t${value}\n")
    }
    pw.close()
  }

}
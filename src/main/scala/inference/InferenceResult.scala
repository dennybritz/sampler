package org.dennybritz.sampler

case class VariableInferenceResult(id: Int, expectation: Double, standardDeviation: Double, lastSample: Double)
case class InferenceResult(variables: List[VariableInferenceResult])
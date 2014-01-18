package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

class SamplingUtilsSpec extends FunSpec {

  val sampleDataInput = DataInput(
    Map(
      0 -> Weight(0, Double.MaxValue, false),
      1 -> Weight(1, Double.MinValue, false)
    ),
    Map(
      0 -> BooleanVariable(0, 0.0, false, true),
      1 -> BooleanVariable(1, 0.0, false, true),
      2 -> BooleanVariable(2, 1.0, true, false)
    ),
    Map(
      0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
      1 -> Factor(1, List(FactorVariable(1, true), FactorVariable(2, true)), 1, ImplyFactorFunction)
    )
  )

  implicit val graphContext = GraphContext.create(sampleDataInput)

  describe("Sampling a single variable") {
    it("should work") {
      SamplingUtils.sampleVariable(sampleDataInput.variablesMap(0))
      assert(graphContext.variableValues.get(0) == 1.0)
      SamplingUtils.sampleVariable(sampleDataInput.variablesMap(1))
      assert(graphContext.variableValues.get(1) == 0.0)
    }
  }

  describe("Sampling multiple variables") {
    it("should work") {
      SamplingUtils.sampleVariables(Set(sampleDataInput.variablesMap(0), sampleDataInput.variablesMap(1)))
      assert(graphContext.variableValues.get(0) == 1.0)
      assert(graphContext.variableValues.get(1) == 0.0)
    }
  }

}

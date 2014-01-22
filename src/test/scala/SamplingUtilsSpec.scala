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
      2 -> BooleanVariable(2, 1.0, true, false),
      3 -> BooleanVariable(3, 0.0, false, true)
    ),
    Map(
      0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
      1 -> Factor(1, List(FactorVariable(1, true), FactorVariable(2, true)), 1, ImplyFactorFunction),
      2 -> Factor(2, List(FactorVariable(3, false)), 0, ImplyFactorFunction)
    )
  )

  implicit val graphContext = GraphContext.create(sampleDataInput)

  describe("Sampling a single variable") {
    it("should work") {
      SamplingUtils.sampleVariable(0)
      assert(graphContext.getVariableValue(0) == 1.0)
      SamplingUtils.sampleVariable(1)
      assert(graphContext.getVariableValue(1) == 0.0)
    }
  }

  describe("Sampling a variable that is negated in the factor funtion") {
    it("should work") {
      SamplingUtils.sampleVariable(3)
      assert(graphContext.getVariableValue(3) == 0.0)
    }
  }

  describe("Sampling multiple variables") {
    it("should work") {
      SamplingUtils.sampleVariables(Set(0,1))
      assert(graphContext.getVariableValue(0) == 1.0)
      assert(graphContext.getVariableValue(1) == 0.0)
    }
  }

}

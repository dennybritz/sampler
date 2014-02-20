package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

class SamplingUtilsSpec extends FunSpec {

  val sampleDataInput = DataInput(
    Vector(
      Weight(0, Double.MaxValue, false),
      Weight(1, Double.MinValue, false)
    ),
    Vector(
      BooleanVariable(0, 0.0, false, true, List(0)),
      BooleanVariable(1, 0.0, false, true, List(1)),
      BooleanVariable(2, 1.0, true, false, List(1)),
      BooleanVariable(3, 0.0, false, true, List(2))
    ),
    Vector(
      Factor(0, List(FactorVariable(0, true, 0)), 0, ImplyFactorFunction),
      Factor(1, List(FactorVariable(2, true, 0), FactorVariable(1, true, 1)), 1, ImplyFactorFunction),
      Factor(2, List(FactorVariable(3, false, 0)), 0, ImplyFactorFunction)
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

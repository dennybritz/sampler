package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

import Matchers._

class SamplerSpec extends FunSpec { 

  describe("Sampling variables with trivial factors") {

      val weights = Vector(Weight(0, Double.MaxValue, false))
      val evidence = (0 until 20) map (i => BooleanVariable(i, 1.0, true, false, List(i)))
      val query = (20 until 100) map (i => BooleanVariable(i, 0.0, false, true, List(i)))
      val factors = (0 until 100) map (i => Factor(i, List(FactorVariable(i, true, 0)), 0, ImplyFactorFunction))
      val dataInput = DataInput(weights.toVector, (evidence ++ query).toVector, factors.toVector)

    it("should work") {
      val context = GraphContext.create(dataInput)
      val sampler = new Sampler(context)
      val result = sampler.calculateMarginals(100, dataInput.variables.toVector)
      assert(result.variables.size === 80)
      for (variable <- query) {
        assert(result.variables.find(_.id == variable.id).get === 
          VariableInferenceResult(variable.id, 1.0, 0.0, 1.0))
      }
    }

    it("should work with a zero weight") {
      val input = dataInput.copy(weights=Vector(Weight(0, 0.0, false)))
      val context = GraphContext.create(input)
      val sampler = new Sampler(context)
      val result = sampler.calculateMarginals(1000, dataInput.variables.toVector)
      assert(result.variables.size === 80)
      for (variable <- query) {
        assert(result.variables.find(_.id == variable.id).get.expectation === (0.5 +- 0.05))
      }
    }

  }

  describe("Sampling three connected variables") {
    val dataInput = DataInput(
      Vector(
        Weight(0, Double.MaxValue, true),
        Weight(1, 0.5, true), 
        Weight(2, Double.MaxValue, true)),
      Vector(
        BooleanVariable(0, 0.0, false, true, List(0,2)),
        BooleanVariable(1, 0.0, false, true, List(1,3)),
        BooleanVariable(2, 0.0, false, true, List(2,3))),
      Vector(
        Factor(0, List(FactorVariable(0, true, 0)), 0, ImplyFactorFunction),
        Factor(1, List(FactorVariable(1, true, 0)), 1, ImplyFactorFunction),
        Factor(2, List(FactorVariable(0, true, 0), FactorVariable(2, true, 1)), 2, ImplyFactorFunction),
        Factor(3, List(FactorVariable(1, true, 0), FactorVariable(2, true, 1)), 2, ImplyFactorFunction)))

    it("should work") {
      val context = GraphContext.create(dataInput)
      val sampler = new Sampler(context)
      val result = sampler.calculateMarginals(100, dataInput.variables.toVector)
      assert(result.variables.size === 3)
      assert(result.variables(0).expectation === (1.0 +- 0.1))
      assert(result.variables(2).expectation === (1.0 +- 0.1))
    }
  }

}


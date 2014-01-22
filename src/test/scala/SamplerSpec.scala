package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

import Matchers._

class SamplerSpec extends FunSpec { 

  describe("Sampling variables with trivial factors") {

      val weights = List(Weight(0, Double.MaxValue, false))
      val evidence = (0 until 20) map (i => BooleanVariable(i, 1.0, true, false))
      val query = (20 until 100) map (i => BooleanVariable(i, 0.0, false, true))
      val factors = (0 until 100) map (i => Factor(i, List(FactorVariable(i, true)), 0, ImplyFactorFunction))
      val dataInput = DataInput(
        weights.map (w => (w.id, w)) toMap,
        (evidence ++ query) map (v => (v.id, v)) toMap,
        factors map (f => (f.id, f)) toMap )

    it("should work") {
      val context = GraphContext.create(dataInput)
      val sampler = new Sampler(context)
      val result = sampler.calculateMarginals(100, dataInput.variablesMap.values.toList)
      assert(result.variables.size === 80)
      for (variable <- query) {
        assert(result.variables.find(_.id == variable.id).get === 
          VariableInferenceResult(variable.id, 1.0, 0.0, 1.0))
      }
    }

    it("should work with a zero weight") {
      val input = dataInput.copy(weightsMap=Map(0 -> Weight(0, 0.0, false)))
      val context = GraphContext.create(input)
      val sampler = new Sampler(context)
      val result = sampler.calculateMarginals(1000, dataInput.variablesMap.values.toList)
      assert(result.variables.size === 80)
      for (variable <- query) {
        assert(result.variables.find(_.id == variable.id).get.expectation === (0.5 +- 0.05))
      }
    }

  }


  describe("Sampling three connected variables") {

    val dataInput = DataInput(
      Map(
        0 -> Weight(0, Double.MaxValue, true),
        1 -> Weight(1, 0.5, true), 
        2 -> Weight(1, Double.MaxValue, true)),
      Map(
        0 -> BooleanVariable(0, 0.0, false, true),
        1 -> BooleanVariable(1, 0.0, false, true),
        2 -> BooleanVariable(2, 0.0, false, true)),
      Map(
        0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
        1 -> Factor(1, List(FactorVariable(1, true)), 1, ImplyFactorFunction),
        2 -> Factor(2, List(FactorVariable(2, true), FactorVariable(0, true)), 2, ImplyFactorFunction),
        3 -> Factor(3, List(FactorVariable(2, true), FactorVariable(1, true)), 2, ImplyFactorFunction)))

    it("should work") {
      val context = GraphContext.create(dataInput)
      val sampler = new Sampler(context)
      val result = sampler.calculateMarginals(1000, dataInput.variablesMap.values.toList)
      assert(result.variables.size === 3)
      assert(result.variables(0).expectation === (1.0 +- 0.1))
      assert(result.variables(2).expectation === (1.0 +- 0.1))
    }


  }




}
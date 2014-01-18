package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

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
      assert(result.size === 80)
      for (variable <- query) {
        assert(result(variable.id) === VariableInferenceResult(1.0, 0.0, 1.0))
      }
    }

  }


}
package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

class GraphContextSpec extends FunSpec {

  val context = GraphContext.create(DataInput(
    Map(0 -> Weight(0, Double.MaxValue, true)),
    Map(0 -> BooleanVariable(0, 0.0, false, true),
      1 -> BooleanVariable(0, 1.0, false, true)),
    Map(0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction))

  ))

  describe("Getting variable values") {
    
    it ("should work for positive variables") {
      assert(context.getVariableValue(0) === 0.0)
      assert(context.getVariableValue(1) === 1.0)
    }
    
    it ("should work for negated variables") {
      assert(context.getVariableValue(0, false) === 1.0)
      assert(context.getVariableValue(1, false) === 0.0)
    }
    
  }

}
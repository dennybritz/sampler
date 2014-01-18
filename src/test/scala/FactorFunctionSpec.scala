package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

class FactorFunctionSpec extends FunSpec {

  describe("Imply Factor Function") {
    
    it("should evaluate to the value for single variable") {
      assert(ImplyFactorFunction.evaluate(List(1.0)) == 1.0)
      assert(ImplyFactorFunction.evaluate(List(0.0)) == 0.0)
    }

    it("should evaluate to true for a false body") {
      assert(ImplyFactorFunction.evaluate(List(1.0, 0.0)) == 1.0)
      assert(ImplyFactorFunction.evaluate(List(0.0, 0.0)) == 1.0)
      assert(ImplyFactorFunction.evaluate(List(1.0, 0.0, 0.0)) == 1.0)
      assert(ImplyFactorFunction.evaluate(List(0.0, 0.0, 0.0)) == 1.0)
    } 

    it("should evaluate to true for a true body and true head") {
      assert(ImplyFactorFunction.evaluate(List(1.0, 1.0)) == 1.0)
      assert(ImplyFactorFunction.evaluate(List(1.0, 1.0, 1.0)) == 1.0)
    }

    it("should evaluate to false for a true body and false head") {
      assert(ImplyFactorFunction.evaluate(List(0.0, 1.0)) == 0.0)
      assert(ImplyFactorFunction.evaluate(List(0.0, 1.0, 1.0)) == 0.0)
    }

  }

}

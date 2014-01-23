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
      assert(ImplyFactorFunction.evaluate(List(1.0, 1.0, 0.0)) == 1.0)
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

  describe("The OR factor function") {

    it("should be true with 0 variables") {
      assert(OrFactorFunction.evaluate(Nil) == 1.0)
    }

    it("should work with 1 variable") {
      assert(OrFactorFunction.evaluate(List(1.0)) == 1.0)
      assert(OrFactorFunction.evaluate(List(0.0)) == 0.0)
    }

    it("should work with 2 variables") {
      assert(OrFactorFunction.evaluate(List(1.0, 0.0)) == 1.0)
      assert(OrFactorFunction.evaluate(List(1.0, 1.0)) == 1.0)
      assert(OrFactorFunction.evaluate(List(0.0, 0.0)) == 0.0)
    }

    it("should work more than 2 variables") {
      assert(OrFactorFunction.evaluate(List(1.0, 0.0, 0.0, 0.0)) == 1.0)
      assert(OrFactorFunction.evaluate(List(0.0, 0.0, 1.0, 0.0)) == 1.0)
      assert(OrFactorFunction.evaluate(List(0.0, 0.0, 0.0, 0.0)) == 0.0)
    }

  }

  describe("The AND factor function") {

    it("should be true with 0 variables") {
      assert(AndFactorFunction.evaluate(Nil) == 1.0)
    }

    it("should work with 1 variable") {
      assert(AndFactorFunction.evaluate(List(1.0)) == 1.0)
      assert(AndFactorFunction.evaluate(List(0.0)) == 0.0)
    }

    it("should work with 2 variables") {
      assert(AndFactorFunction.evaluate(List(1.0, 0.0)) == 0.0)
      assert(AndFactorFunction.evaluate(List(1.0, 1.0)) == 1.0)
      assert(AndFactorFunction.evaluate(List(0.0, 0.0)) == 0.0)
    }

    it("should work more than 2 variables") {
      assert(AndFactorFunction.evaluate(List(1.0, 0.0, 0.0, 0.0)) == 0.0)
      assert(AndFactorFunction.evaluate(List(0.0, 0.0, 1.0, 0.0)) == 0.0)
      assert(AndFactorFunction.evaluate(List(1.0, 1.0, 1.0, 1.0)) == 1.0)
    }
  }

  describe("The EQUAL factor function") {

    it("should throw an error with less than 2 variables") {
      intercept[RuntimeException] {
        EqualFactorFunction.evaluate(Nil)
      }
      intercept[RuntimeException] {
        EqualFactorFunction.evaluate(List(1.0))
      }
    }

    it("should throw an error with more than 2 variables") {
      intercept[RuntimeException] {
        EqualFactorFunction.evaluate(List(1.0, 0.0, 1.0))
      }
    }

    it("should work with 2 variables") {
      assert(EqualFactorFunction.evaluate(List(0.0, 0.0)) == 1.0)
      assert(EqualFactorFunction.evaluate(List(0.0, 1.0)) == 0.0)
      assert(EqualFactorFunction.evaluate(List(1.0, 0.0)) == 0.0)
      assert(EqualFactorFunction.evaluate(List(1.0, 1.0)) == 1.0) 
    }

  }


  describe("The IS_TRUE factor function") {

    it("should throw an error with other than 1 variables") {
      intercept[RuntimeException] {
        IsTrueFactorFunction.evaluate(Nil)
      }
      intercept[RuntimeException] {
        IsTrueFactorFunction.evaluate(List(1.0, 0.0))
      }
    }

    it("should work with 1 variable") {
      assert(IsTrueFactorFunction.evaluate(List(1.0)) == 1.0)
      assert(IsTrueFactorFunction.evaluate(List(0.0)) == 0.0)
    }

  }

}

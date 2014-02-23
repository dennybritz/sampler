package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

class LearningSpec extends FunSpec {

  describe("Learning one positive weight with one evidence value") {

    val dataInput = DataInput(
      Vector(Weight(0, 0.0, false)),
      Vector(BooleanVariable(0, 1.0, true, false, List(0)), BooleanVariable(1, 0.0, false, true, List(0))),
      Vector(Factor(0, List(FactorVariable(0, true, 0)), 0, ImplyFactorFunction),
        Factor(1, List(FactorVariable(1, true, 0)), 0, ImplyFactorFunction)))

    it("should work") {
      val context = GraphContext.create(dataInput)
      val learner = new Learner(context)
      val result = learner.learnWeights(100, 10, 0.1, 0.01, 0.96)
      assert(result(0) > 0)
    }
  }

  describe("Learning fixed weights") {
    
    val dataInput = DataInput(
      Vector(Weight(0, 0.0, true)),
      Vector(BooleanVariable(0, 1.0, true, false, List(0)), BooleanVariable(1, 0.0, false, true, List(1))),
      Vector(Factor(0, List(FactorVariable(0, true, 0)), 0, ImplyFactorFunction),
        Factor(1, List(FactorVariable(1, true, 0)), 0, ImplyFactorFunction)))

    it("should not be done") {
      val context = GraphContext.create(dataInput)
      val learner = new Learner(context)
      val result = learner.learnWeights(100, 10, 0.1, 0.01, 0.96)
      assert(result.toSeq == Array(0.0).toSeq)
    }
  }

  describe("Learning one negative weight with one evidence value") {
    val dataInput = DataInput(
      Vector(Weight(0, 0.0, false)),
      Vector(BooleanVariable(0, 0.0, true, false, List(0)), BooleanVariable(1, 0.0, false, true, List(1))),
      Vector(Factor(0, List(FactorVariable(0, true, 0)), 0, ImplyFactorFunction),
        Factor(1, List(FactorVariable(1, true, 0)), 0, ImplyFactorFunction)))

    it("should work") {
      val context = GraphContext.create(dataInput)
      val learner = new Learner(context)
      val result = learner.learnWeights(100, 10, 0.1, 0.01, 0.96)
      assert(result(0) < 0)
    }
  }

  // Two positive, one negative evidence. One weight.
  describe("Learning one positive weight with multiple evidence values") {
    val dataInput = DataInput(
      Vector(Weight(0, 0.0, false)),
      Vector(
        BooleanVariable(0, 0.0, true, false, List(0)), 
        BooleanVariable(1, 0.0, false, true, List(1)),
        BooleanVariable(2, 1.0, true, false, List(2)), 
        BooleanVariable(3, 1.0, true, false, List(3))),
      Vector(
        Factor(0, List(FactorVariable(0, true, 0)), 0, ImplyFactorFunction),
        Factor(1, List(FactorVariable(1, true, 0)), 0, ImplyFactorFunction),
        Factor(2, List(FactorVariable(2, true, 0)), 0, ImplyFactorFunction),
        Factor(3, List(FactorVariable(3, true, 0)), 0, ImplyFactorFunction)))

    it("should work") {
      val context = GraphContext.create(dataInput)
      val learner = new Learner(context)
      val result = learner.learnWeights(100, 10, 0.1, 0.01, 0.96)
      Console.println(result.mkString)
      assert(result(0) > 0)
    }
  }

  // Two negative, one positive evidence value. One weight.
  describe("Learning one negative weight with multiple evidence values") {
    val dataInput = DataInput(
      Vector(Weight(0, 0.0, false)),
      Vector(
        BooleanVariable(0, 1.0, true, false, List(0)), 
        BooleanVariable(1, 0.0, false, true, List(1)),
        BooleanVariable(2, 0.0, true, false, List(2)), 
        BooleanVariable(3, 0.0, true, false, List(3))),
      Vector(
        Factor(0, List(FactorVariable(0, true, 9)), 0, ImplyFactorFunction),
        Factor(1, List(FactorVariable(1, true, 0)), 0, ImplyFactorFunction),
        Factor(2, List(FactorVariable(2, true, 0)), 0, ImplyFactorFunction),
        Factor(3, List(FactorVariable(3, true, 0)), 0, ImplyFactorFunction)))

    it("should work") {
      val context = GraphContext.create(dataInput)
      val learner = new Learner(context)
      val result = learner.learnWeights(100, 10, 0.1, 0.01, 0.96)
      assert(result(0) < 0)
    }
  }

  describe("Learning multiple positive weights") {

    it("should work") {
      // 10 weights
      val weights = (0 until 10) map( i => Weight(i, 0.0, false))
      // 50 positive evidence variables, 50 query variables
      val evidenceVariables = (0 until 50) map (i => BooleanVariable(i, 1.0, true, false, List(i)))
      val queryVariables = (50 until 100) map (i => BooleanVariable(i, 0.0, false, true, List(i)))
      val variables = evidenceVariables ++ queryVariables
      // 100 factors, one connecting to each variable, 5 factors share one weight
      val factors = (0 until 100).map { factorId =>
         Factor(factorId, List(FactorVariable(factorId, true, 0)), factorId % 10, ImplyFactorFunction)
      }
      val dataInput = DataInput(weights.toVector, variables.toVector, factors.toVector)
      val context = GraphContext.create(dataInput)
      val learner = new Learner(context)
      val result = learner.learnWeights(100, 10, 0.1, 0.01, 0.96)
      weights.foreach { weight =>
        assert(result(weight.id) > 0)
      }
    }
  }

}


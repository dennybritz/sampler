package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

class LearningSpec extends FunSpec {

  describe("Learning one positive weight with one evidence value") {

    val dataInput = DataInput(
      Map(0 -> Weight(0, 0.0, false)),
      Map(0 -> BooleanVariable(0, 1.0, true, false), 1 -> BooleanVariable(1, 0.0, false, true)),
      Map(0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
        1 -> Factor(1, List(FactorVariable(1, true)), 0, ImplyFactorFunction)))

    it("should work") {
      val context = GraphContext.create(dataInput)
      val learner = new Learner(context)
      val result = learner.learnWeights(100, 10, 0.1, 0.01, 0.96)
      assert(result(0) > 0)
    }
  }

  describe("Learning one negative weight with one evidence value") {
    val dataInput = DataInput(
      Map(0 -> Weight(0, 0.0, false)),
      Map(0 -> BooleanVariable(0, 0.0, true, false), 1 -> BooleanVariable(1, 0.0, false, true)),
      Map(0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
        1 -> Factor(1, List(FactorVariable(1, true)), 0, ImplyFactorFunction)))

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
      Map(0 -> Weight(0, 0.0, false)),
      Map(
        0 -> BooleanVariable(0, 0.0, true, false), 
        1 -> BooleanVariable(1, 0.0, false, true),
        2 -> BooleanVariable(2, 1.0, true, false), 
        3 -> BooleanVariable(3, 1.0, true, false)),
      Map(
        0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
        1 -> Factor(1, List(FactorVariable(1, true)), 0, ImplyFactorFunction),
        2 -> Factor(2, List(FactorVariable(2, true)), 0, ImplyFactorFunction),
        3 -> Factor(3, List(FactorVariable(3, true)), 0, ImplyFactorFunction)))

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
      Map(0 -> Weight(0, 0.0, false)),
      Map(0 -> BooleanVariable(0, 1.0, true, false), 1 -> BooleanVariable(1, 0.0, false, true),
        2 -> BooleanVariable(2, 0.0, true, false), 3 -> BooleanVariable(3, 0.0, true, false)),
      Map(0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
        1 -> Factor(1, List(FactorVariable(1, true)), 0, ImplyFactorFunction),
        2 -> Factor(2, List(FactorVariable(2, true)), 0, ImplyFactorFunction),
        3 -> Factor(3, List(FactorVariable(3, true)), 0, ImplyFactorFunction)))

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
      val evidenceVariables = (0 until 50) map (i => BooleanVariable(i, 1.0, true, false))
      val queryVariables = (50 until 100) map (i => BooleanVariable(i, 0.0, false, true))
      val variables = evidenceVariables ++ queryVariables
      // 100 factors, one connecting to each variable, 5 factors share one weight
      val factors = (0 until 100).map { factorId =>
         Factor(factorId, List(FactorVariable(factorId, true)), factorId % 10, ImplyFactorFunction)
      }
      val dataInput = DataInput(weights.map(w => (w.id, w)).toMap, 
        variables.map(v => (v.id, v)).toMap,
        factors.map(f => (f.id, f)).toMap)
      val context = GraphContext.create(dataInput)
      val learner = new Learner(context)
      val result = learner.learnWeights(100, 10, 0.1, 0.01, 0.96)
      weights.foreach { weight =>
        assert(result(weight.id) > 0)
      }
    }
  }

}


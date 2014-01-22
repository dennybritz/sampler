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

  }


  describe("Two variable") {

    val weights = List(Weight(0, 1.0, true), Weight(1, 10.0, true))
    val evidence = List()
    var query = List(BooleanVariable(0, 0.0, false, true), BooleanVariable(1, 0.0, false, true))
    val factors = List( Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
                        Factor(1, List(FactorVariable(1, true), FactorVariable(0, true)), 1, ImplyFactorFunction)
    )
    val dataInput = DataInput(
        weights.map (w => (w.id, w)) toMap,
        (evidence ++ query) map (v => (v.id, v)) toMap,
        factors map (f => (f.id, f)) toMap )

    it("should work") {
      val context = GraphContext.create(dataInput)
      val sampler = new Sampler(context)
      val result = sampler.calculateMarginals(100, dataInput.variablesMap.values.toList)
      assert(result.variables.size === 2)
      for (variable <- query) {
        println(result.variables.find(_.id == variable.id).get);
        //assert(result.variables.find(_.id == variable.id).get === 
        //  VariableInferenceResult(variable.id, 1.0, 0.0, 1.0))
      }
    }


  }


  describe("Three variable") {

    val weights = List(Weight(0, 9, true), Weight(1, 2, true), Weight(2, 0, true), Weight(3, 0.4, true),  Weight(4, 10.0, true))
    val evidence = List()
    var query = List(BooleanVariable(0, 0.0, false, true), BooleanVariable(1, 0.0, false, true), BooleanVariable(3, 0.0, false, true))
    val factors = List( Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
                        Factor(1, List(FactorVariable(0, true)), 1, ImplyFactorFunction),
                        Factor(2, List(FactorVariable(1, true)), 2, ImplyFactorFunction),
                        Factor(3, List(FactorVariable(1, true)), 3, ImplyFactorFunction),
                        Factor(4, List(FactorVariable(3, true), FactorVariable(0, true)), 4, ImplyFactorFunction),
                        Factor(5, List(FactorVariable(3, true), FactorVariable(1, true)), 4, ImplyFactorFunction)
    )
    val dataInput = DataInput(
        weights.map (w => (w.id, w)) toMap,
        (evidence ++ query) map (v => (v.id, v)) toMap,
        factors map (f => (f.id, f)) toMap )

    it("should work") {
      val context = GraphContext.create(dataInput)
      val sampler = new Sampler(context)
      val result = sampler.calculateMarginals(100, dataInput.variablesMap.values.toList)
      assert(result.variables.size === 3)


      println(result.variables.find(_.id == 0).get);
      println(result.variables.find(_.id == 1).get);
      println(result.variables.find(_.id == 3).get);

      //for (variable <- query) {
      //  println(result.variables.find(_.id == variable.id).get);
      //  //assert(result.variables.find(_.id == variable.id).get === 
      //  //  VariableInferenceResult(variable.id, 1.0, 0.0, 1.0))
      //}
    }


  }



}
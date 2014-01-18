package org.dennybritz.sampler.test

import org.scalatest._
import org.dennybritz.sampler._

class DeepDiveInputParserSpec extends FunSpec {

  describe("Parsing the weights") {

    it("should work on valid files") {
      val weightsFile = getClass.getResource("/deepdive_weights.txt").getFile
      val result = DeepDiveInputParser.parseWeights(weightsFile)
      assert(result.size == 2)
      assert(result == List(
        Weight(1013, 0.0, false), Weight(585, 0.0, false)
      ))
    }

    it("should throw an exception on invalid files") {
      val NotWeightsFile = getClass.getResource("/deepdive_variables.txt").getFile
      intercept[RuntimeException] {
        DeepDiveInputParser.parseWeights(NotWeightsFile)
      }
    }

  }

  describe("Parsing the variable factor map file") {

    it("should work for valid files") {
      val variableMapFile = getClass.getResource("/deepdive_variables.txt").getFile
      val result = DeepDiveInputParser.parseVariableFactorMap(variableMapFile)
      assert(result.variables.toSet == 
        List(BooleanVariable(0, 0.0, false, true), BooleanVariable(1, 1.0, true, false),
          BooleanVariable(2, 1.0, true, false)).toSet)
      assert(result.factorMap == Map(
        0 -> List(FactorVariable(0, true)),
        1 -> List(FactorVariable(1, true), FactorVariable(2, true))
      ))
    }

    it("should throw an exception for invalid input files") {
      val notVariableMapFile = getClass.getResource("/deepdive_weights.txt").getFile
      intercept[RuntimeException] {
        DeepDiveInputParser.parseVariableFactorMap(notVariableMapFile)
      }
    }

  }

  describe("Parsing the factors file") {

    it("should work with valid files") {
      val factorsFile = getClass.getResource("/deepdive_factors.txt").getFile
      val factorVariableMap = Map(
        251 -> List(FactorVariable(0, true), FactorVariable(1, true)),
        2026 -> List(FactorVariable(2, true)))
      val result = DeepDiveInputParser.parseFactors(factorsFile, factorVariableMap)
      assert(result == List(
        Factor(251, List(FactorVariable(0, true), FactorVariable(1, true)), 187, ImplyFactorFunction),
        Factor(2026, List(FactorVariable(2, true)), 382, ImplyFactorFunction)
      ))
    }

    it("should prune factors with no variables") {
      val factorsFile = getClass.getResource("/deepdive_factors.txt").getFile
      val factorVariableMap = Map(
        251 -> List(FactorVariable(0, true), FactorVariable(1, true)))
      val result = DeepDiveInputParser.parseFactors(factorsFile, factorVariableMap)
      assert(result == List(
        Factor(251, List(FactorVariable(0, true), FactorVariable(1, true)), 187, ImplyFactorFunction)
      ))
    }

    it("should throw an exception for invalid input files") {
      val notFactorsFile = getClass.getResource("/deepdive_variables.txt").getFile
      intercept[RuntimeException] {
        DeepDiveInputParser.parseFactors(notFactorsFile, Map())
      }
    }

  }

}

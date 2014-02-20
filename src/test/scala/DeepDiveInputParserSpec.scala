// package org.dennybritz.sampler.test

// import org.scalatest._
// import org.dennybritz.sampler._

// class DeepDiveInputParserSpec extends FunSpec {

//   describe("Parsing the weights") {

//     it("should work on valid files") {
//       val weightsFile = getClass.getResource("/deepdive_weights.txt").getFile
//       val result = DeepDiveInputParser.parseWeights(weightsFile)
//       assert(result.size == 2)
//       assert(result == Map(
//         1013 -> Weight(1013, 0.0, false), 585 -> Weight(585, 0.0, false)
//       ))
//     }

//     it("should throw an exception on invalid files") {
//       val NotWeightsFile = getClass.getResource("/deepdive_variables.txt").getFile
//       intercept[RuntimeException] {
//         DeepDiveInputParser.parseWeights(NotWeightsFile)
//       }
//     }

//   }

//   describe("Parsing the variable factor map file") {

//     it("should work for valid files") {
//       val variableMapFile = getClass.getResource("/deepdive_variables.txt").getFile
//       val result = DeepDiveInputParser.parseVariableFactorMap(variableMapFile)
//       assert(result.variables == 
//         Map(0 -> BooleanVariable(0, 0.0, false, true), 
//           1 -> BooleanVariable(1, 1.0, true, false),
//           2 -> BooleanVariable(2, 1.0, true, false)))
//       assert(result.factorMap == 
//         Map(0 -> List(FactorVariable(0, true)),
//         1 -> List(FactorVariable(1, true), FactorVariable(2, true))
//       ))
//     }

//     it("should throw an exception for invalid input files") {
//       val notVariableMapFile = getClass.getResource("/deepdive_weights.txt").getFile
//       intercept[RuntimeException] {
//         DeepDiveInputParser.parseVariableFactorMap(notVariableMapFile)
//       }
//     }

//   }

//   describe("Parsing the factors file") {

//     it("should work with valid files") {
//       val factorsFile = getClass.getResource("/deepdive_factors.txt").getFile
//       val factorVariableMap = Map(
//         251 -> List(FactorVariable(0, true), FactorVariable(1, true)),
//         2026 -> List(FactorVariable(2, true)))
//       val result = DeepDiveInputParser.parseFactors(factorsFile, factorVariableMap)
//       assert(result == Map(
//         251 -> Factor(251, List(FactorVariable(0, true), FactorVariable(1, true)), 187, ImplyFactorFunction),
//         2026 -> Factor(2026, List(FactorVariable(2, true)), 382, ImplyFactorFunction)
//       ))
//     }

//     it("should prune factors with no variables") {
//       val factorsFile = getClass.getResource("/deepdive_factors.txt").getFile
//       val factorVariableMap = Map(
//         251 -> List(FactorVariable(0, true), FactorVariable(1, true)))
//       val result = DeepDiveInputParser.parseFactors(factorsFile, factorVariableMap)
//       assert(result == Map(
//         251 -> Factor(251, List(FactorVariable(0, true), FactorVariable(1, true)), 187, ImplyFactorFunction)
//       ))
//     }

//     it("should throw an exception for invalid input files") {
//       val notFactorsFile = getClass.getResource("/deepdive_variables.txt").getFile
//       intercept[RuntimeException] {
//         DeepDiveInputParser.parseFactors(notFactorsFile, Map())
//       }
//     }

//     it("should parse different factor functions") {
//       val factorsFile = getClass.getResource("/deepdive_factor_functions.txt").getFile
//       val factorVariableMap = Map(
//         0 -> List(FactorVariable(0, true)), 
//         1 -> List(FactorVariable(0, true)), 
//         2 -> List(FactorVariable(0, true)), 
//         3 -> List(FactorVariable(0, true)), 
//         4 -> List(FactorVariable(0, true)))
//       val result = DeepDiveInputParser.parseFactors(factorsFile, factorVariableMap)
//       assert(result == Map(
//         0 -> Factor(0, List(FactorVariable(0, true)), 0, ImplyFactorFunction),
//         1 -> Factor(1, List(FactorVariable(0, true)), 1, AndFactorFunction),
//         2 -> Factor(2, List(FactorVariable(0, true)), 2, OrFactorFunction),
//         3 -> Factor(3, List(FactorVariable(0, true)), 3, IsTrueFactorFunction),
//         4 -> Factor(4, List(FactorVariable(0, true)), 4, EqualFactorFunction)
//       ))

//     }

//   }

// }

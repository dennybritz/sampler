package org.dennybritz.sampler.test

import java.io.{File, FileInputStream, FileOutputStream}
import org.scalatest._
import org.dennybritz.sampler._
import org.deepdive.serialization.FactorGraphProtos

class BinaryInputParserSpec extends FunSpec {

  describe("Parsing a factor graph file") {

    it("should work on valid files") {
      val weightsFile = getClass.getResource("/binary/weights").getFile
      val variablesFile = getClass.getResource("/binary/variables").getFile
      val factorsFile = getClass.getResource("/binary/factors").getFile
      val edgesFile = getClass.getResource("/binary/edges").getFile
      val metaFile = getClass.getResource("/binary/meta.csv").getFile
      val parseResult = BinaryInputParser.parse(
        new BinaryInput(weightsFile, variablesFile, factorsFile, edgesFile, metaFile))

      assert(parseResult.weights(0).id === 0)
      assert(parseResult.weights(0).isFixed === false)
      assert(parseResult.weights(0).value === 0.0)
      assert(parseResult.weights(1).id === 1)
      assert(parseResult.weights(1).isFixed === true)
      assert(parseResult.weights(1).value === 10.0)

      assert(parseResult.variables(0).id === 0)
      assert(parseResult.variables(0).factorIds.size === 2)
      assert(parseResult.variables(0).isEvidence === false)
      assert(parseResult.variables(1).id === 1)
      assert(parseResult.variables(1).isEvidence === true)
      assert(parseResult.variables(1).value === 1.0)
      assert(parseResult.variables(1).factorIds.size === 1)

      assert(parseResult.factors(0).id === 0)
      assert(parseResult.factors(0).weightId === 0)
      assert(parseResult.factors(0).variables.size === 1)
      assert(parseResult.factors(0).function === IsTrueFactorFunction)
      assert(parseResult.factors(1).id === 1)
      assert(parseResult.factors(1).weightId === 1)
      assert(parseResult.factors(1).variables.size === 2)
      assert(parseResult.factors(1).function === ImplyFactorFunction)

    }

  }

}

package org.dennybritz.sampler.test

import java.io.{File, FileInputStream, FileOutputStream}
import org.scalatest._
import org.dennybritz.sampler._
import org.deepdive.serialization.FactorGraphProtos

class ProtobufInputParserSpec extends FunSpec {

  describe("Parsing a factor graph file") {

    it("should work on valid files") {
      val weightsFile = getClass.getResource("/graph.weights.pb").getFile
      val variablesFile = getClass.getResource("/graph.variables.pb").getFile
      val factorsFile = getClass.getResource("/graph.factors.pb").getFile
      val edgesFile = getClass.getResource("/graph.edges.pb").getFile
      val parseResult = ProtobufInputParser.parse(
        new ProtobufInput(weightsFile, variablesFile, factorsFile, edgesFile))
    }

    it("should parse weights correctly") {
      val weightBuilder = FactorGraphProtos.Weight.newBuilder

      val tmpFile = File.createTempFile("ProtobufInputParserSpec", "pb")
      val output = new FileOutputStream(tmpFile)

      weightBuilder.setId(0).setIsFixed(false).setDescription("w1")
      weightBuilder.build().writeDelimitedTo(output)
      weightBuilder.setId(1).setIsFixed(true).setInitialValue(10.0).setDescription("w2")
      weightBuilder.build().writeDelimitedTo(output)
      output.close()

      val parseResult = ProtobufInputParser.parse(new ProtobufInput(tmpFile.getCanonicalPath,
        "/dev/null", "/dev/null", "/dev/null"))
      assert(parseResult.weightsMap === Map(
        0 -> Weight(0, 0.0, false),
        1 -> Weight(1, 10.0, true)
      ))
    }

    it("should parse variables correctly") {
      val vBuilder = FactorGraphProtos.Variable.newBuilder

      val tmpFile = File.createTempFile("ProtobufInputParserSpec", "pb")
      val output = new FileOutputStream(tmpFile)

      vBuilder.setId(0).setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN)
      vBuilder.build().writeDelimitedTo(output)
      vBuilder.setId(1).setInitialValue(1.0).setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN)
      vBuilder.build().writeDelimitedTo(output)
      vBuilder.setId(2).setInitialValue(0.0).setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN)
      vBuilder.build().writeDelimitedTo(output)
      output.close()

      val parseResult = ProtobufInputParser.parse(new ProtobufInput(
        "/dev/null", tmpFile.getCanonicalPath, "/dev/null", "/dev/null"))
      assert(parseResult.variablesMap === Map(
        0 -> BooleanVariable(0, 0.0, false, true),
        1 -> BooleanVariable(1, 1.0, true, false),
        2 -> BooleanVariable(2, 0.0, true, false)
      ))
    }

    it("should correctly parse factors") {

      val tmpFileWeights = File.createTempFile("weights", "pb")
      val tmpFileVariables = File.createTempFile("variables", "pb")
      val tmpFileFactors = File.createTempFile("factors", "pb")
      val tmpFileEdges = File.createTempFile("edges", "pb")
      val oWeights = new FileOutputStream(tmpFileWeights)
      val oVariables = new FileOutputStream(tmpFileVariables)
      val oFactors = new FileOutputStream(tmpFileFactors)
      val oEdges = new FileOutputStream(tmpFileEdges)

      FactorGraphProtos.Weight.newBuilder.setId(0)
        .setIsFixed(false).setDescription("w1").build().writeDelimitedTo(oWeights)
      FactorGraphProtos.Weight.newBuilder.setId(1)
        .setIsFixed(false).setDescription("w2").build().writeDelimitedTo(oWeights)
      FactorGraphProtos.Variable.newBuilder.setId(0)
        .setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN).build()
        .writeDelimitedTo(oVariables)
      FactorGraphProtos.Variable.newBuilder.setId(1).setInitialValue(1.0)
        .setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN).build()
        .writeDelimitedTo(oVariables)
      FactorGraphProtos.Variable.newBuilder.setId(2)
        .setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN).build()
        .writeDelimitedTo(oVariables)
      FactorGraphProtos.Factor.newBuilder.setId(0).setWeightId(0)
        .setFactorFunction(FactorGraphProtos.Factor.FactorFunctionType.IMPLY).build()
        .writeDelimitedTo(oFactors)
      FactorGraphProtos.Factor.newBuilder.setId(1).setWeightId(1)
        .setFactorFunction(FactorGraphProtos.Factor.FactorFunctionType.IMPLY).build()
        .writeDelimitedTo(oFactors)
      FactorGraphProtos.GraphEdge.newBuilder.setVariableId(1).setFactorId(0)
        .setPosition(1).build().writeDelimitedTo(oEdges)
      FactorGraphProtos.GraphEdge.newBuilder.setVariableId(0).setFactorId(0)
        .setPosition(0).build().writeDelimitedTo(oEdges)
      FactorGraphProtos.GraphEdge.newBuilder.setVariableId(2).setFactorId(1)
        .setPosition(0).build().writeDelimitedTo(oEdges)

      oWeights.close()
      oVariables.close()
      oFactors.close()
      oEdges.close()

      val parseResult = ProtobufInputParser.parse(new ProtobufInput(
        tmpFileWeights.getCanonicalPath, tmpFileVariables.getCanonicalPath, 
        tmpFileFactors.getCanonicalPath, tmpFileEdges.getCanonicalPath))
      assert(parseResult.factorsMap === Map(
        0 -> Factor(0, List(FactorVariable(0, true), FactorVariable(1, true)), 0, ImplyFactorFunction),
        1 -> Factor(1, List(FactorVariable(2, true)), 1, ImplyFactorFunction)
      ))
    }

  }

}

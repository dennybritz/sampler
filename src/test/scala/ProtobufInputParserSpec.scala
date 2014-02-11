package org.dennybritz.sampler.test

import java.io.{File, FileInputStream, FileOutputStream}
import org.scalatest._
import org.dennybritz.sampler._
import org.deepdive.serialization.FactorGraphProtos

class ProtobufInputParserSpec extends FunSpec {

  describe("Parsing a factor graph file") {

    it("should work on valid files") {
      val file = getClass.getResource("/deepdive_graph.pb").getFile
      val parseResult = ProtobufInputParser.parse(new ProtobufInput(file))
    }

    it("should parse weights correctly") {
      val graph = FactorGraphProtos.FactorGraph.newBuilder
      val weightBuilder = FactorGraphProtos.Weight.newBuilder

      weightBuilder.setId(0).setIsFixed(false).setDescription("w1")
      graph.addWeight(weightBuilder.build())
      weightBuilder.setId(1).setIsFixed(true).setInitialValue(10.0).setDescription("w2")
      graph.addWeight(weightBuilder.build())

      val tmpFile = File.createTempFile("ProtobufInputParserSpec", "pb")
      val output = new FileOutputStream(tmpFile)
      graph.build().writeTo(output)
      output.close()

      val parseResult = ProtobufInputParser.parse(new ProtobufInput(tmpFile.getCanonicalPath))
      assert(parseResult.weightsMap === Map(
        0 -> Weight(0, 0.0, false),
        1 -> Weight(1, 10.0, true)
      ))
    }

    it("should parse variables correctly") {
      val graph = FactorGraphProtos.FactorGraph.newBuilder
      val vBuilder = FactorGraphProtos.Variable.newBuilder

      vBuilder.setId(0).setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN)
      graph.addVariable(vBuilder.build())
      vBuilder.setId(1).setInitialValue(1.0).setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN)
      graph.addVariable(vBuilder.build())
      vBuilder.setId(2).setInitialValue(0.0).setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN)
      graph.addVariable(vBuilder.build())

      val tmpFile = File.createTempFile("ProtobufInputParserSpec", "pb")
      val output = new FileOutputStream(tmpFile)
      graph.build().writeTo(output)
      output.close()

      val parseResult = ProtobufInputParser.parse(new ProtobufInput(tmpFile.getCanonicalPath))
      assert(parseResult.variablesMap === Map(
        0 -> BooleanVariable(0, 0.0, false, true),
        1 -> BooleanVariable(1, 1.0, true, false),
        2 -> BooleanVariable(2, 0.0, true, false)
      ))
    }

    it("should correctly parse factors") {
      val graph = FactorGraphProtos.FactorGraph.newBuilder

      graph.addWeight(FactorGraphProtos.Weight.newBuilder.setId(0)
        .setIsFixed(false).setDescription("w1").build)
      graph.addWeight(FactorGraphProtos.Weight.newBuilder.setId(1)
        .setIsFixed(false).setDescription("w2").build)
      graph.addVariable(FactorGraphProtos.Variable.newBuilder.setId(0)
        .setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN).build)
      graph.addVariable(FactorGraphProtos.Variable.newBuilder.setId(1).setInitialValue(1.0)
        .setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN).build)
      graph.addVariable(FactorGraphProtos.Variable.newBuilder.setId(2)
        .setDataType(FactorGraphProtos.Variable.VariableDataType.BOOLEAN).build)
      graph.addFactor(FactorGraphProtos.Factor.newBuilder.setId(0).setWeightId(0)
        .setFactorFunction(FactorGraphProtos.Factor.FactorFunctionType.IMPLY).build)
      graph.addFactor(FactorGraphProtos.Factor.newBuilder.setId(1).setWeightId(1)
        .setFactorFunction(FactorGraphProtos.Factor.FactorFunctionType.IMPLY).build)
      graph.addEdge(FactorGraphProtos.GraphEdge.newBuilder.setVariableId(1).setFactorId(0)
        .setPosition(1).build)
      graph.addEdge(FactorGraphProtos.GraphEdge.newBuilder.setVariableId(0).setFactorId(0)
        .setPosition(0).build)
      graph.addEdge(FactorGraphProtos.GraphEdge.newBuilder.setVariableId(2).setFactorId(1)
        .setPosition(0).build)

      val tmpFile = File.createTempFile("ProtobufInputParserSpec", "pb")
      val output = new FileOutputStream(tmpFile)
      graph.build().writeTo(output)
      output.close()

      val parseResult = ProtobufInputParser.parse(new ProtobufInput(tmpFile.getCanonicalPath))
      assert(parseResult.factorsMap === Map(
        0 -> Factor(0, List(FactorVariable(0, true), FactorVariable(1, true)), 0, ImplyFactorFunction),
        1 -> Factor(1, List(FactorVariable(2, true)), 1, ImplyFactorFunction)
      ))
    }

  }

}

package org.dennybritz.sampler

import java.io.{FileInputStream, DataInputStream}
import scala.io.Source
// import scala.collection.mutable.{Map => MMap, ArrayBuffer}
// import scala.util.matching._
// import org.deepdive.serialization.FactorGraphProtos
// import scala.collection.JavaConversions._

case class BinaryInput(inputWeightsPath: String, inputVariablesPath: String, inputFactorsPath: String,
  inputEdgesPath: String, metaDataPath: String)

object BinaryInputParser extends InputParser[BinaryInput] with Logging {
    
  def parse(input: BinaryInput) : DataInput = {

      // Load the Metadata
      val Array(numWeights, numVariables, numFactors, numEdges, weightsFile, variablesFile, 
        factorsFile, edgesFile) = Source.fromFile(input.metaDataPath).getLines.toList.head.split(",")

       // Load the weights
      log.debug(s"Parsing num_weights=${numWeights}")
      val weightStream = new DataInputStream(new FileInputStream(input.inputWeightsPath))
      val weights = (0 until numWeights.toInt).map { i =>
        val weightId = weightStream.readLong().toInt
        val isFixed = weightStream.readBoolean()
        val initialValue = weightStream.readDouble()
        Weight(weightId, initialValue, isFixed)
      }
      weightStream.close()

       // Load the Variables
      log.debug(s"Parsing num_variables=${numVariables}")
      val variableStream = new DataInputStream(new FileInputStream(input.inputVariablesPath))
      val variables = (0 until numVariables.toInt).map { i =>
        val variableId = variableStream.readLong().toInt
        val isEvidence = variableStream.readBoolean()
        val initialValue = variableStream.readDouble()
        // Note: This sampler only supports Boolean variables
        val dataType = variableStream.readShort()
        val edgeCount = variableStream.readLong()
        val cardinality = variableStream.readLong()
        BooleanVariable(variableId, initialValue, isEvidence, !isEvidence, Nil)
      }.sortBy(_.id).toBuffer
      variableStream.close()

      // Load the factors
      log.debug(s"Parsing num_factors=${numFactors}")
      val factorStream = new DataInputStream(new FileInputStream(input.inputFactorsPath))
      val factors = (0 until numFactors.toInt).map { i =>
        val factorId = factorStream.readLong().toInt
        val weightId = factorStream.readLong().toInt
        val factorFunctionId = factorStream.readShort()
        val factorFunction = factorFunctionId match {
          case 0 => ImplyFactorFunction
          case 1 => OrFactorFunction
          case 2 => AndFactorFunction
          case 3 => EqualFactorFunction
          case 4 => IsTrueFactorFunction
          case _ => 
            log.error(s"Unknown factor function type: ${factorFunctionId}")
            throw new IllegalArgumentException("Unknown factor function type: ${factorFunctionId}")
        }
        val edgeCount = factorStream.readLong()
        Factor(factorId, Nil, weightId, factorFunction)
      }.sortBy(_.id).toBuffer
      factorStream.close()

      // Load the edges
      log.debug(s"Parsing num_edges=${numEdges}")
      val edgeStream = new DataInputStream(new FileInputStream(input.inputEdgesPath))
      val edges = (0 until numEdges.toInt).map { i =>
        val variableId = edgeStream.readLong().toInt
        val factorId = edgeStream.readLong().toInt
        val position = edgeStream.readLong().toInt
        val isPositive = edgeStream.readBoolean()
        val equalPredicate = edgeStream.readLong()

        val oldVariable = variables(variableId)
        variables(variableId) = oldVariable.copy(factorIds = oldVariable.factorIds :+ factorId)

        val oldFactor = factors(factorId)
        val factorVar = FactorVariable(variableId, isPositive, position)
        factors(factorId) = oldFactor.copy(variables= (oldFactor.variables :+ factorVar).sortBy(_.position))
      }
      edgeStream.close()

     DataInput(weights.toVector, variables.toVector, factors.toVector)
  }


}

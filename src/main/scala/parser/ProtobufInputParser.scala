package org.dennybritz.sampler

import java.io.FileInputStream
import scala.io.Source
import scala.collection.mutable.{Map => MMap}
import scala.util.matching._
import org.deepdive.serialization.FactorGraphProtos
import scala.collection.JavaConversions._

case class ProtobufInput(inputWeightsPath: String, inputVariablesPath: String, inputFactorsPath: String,
  inputEdgesPath: String)
//case class VariableFactorMap(variables: Map[Int, _ <: Variable], factorMap: Map[Int, List[FactorVariable]])

object ProtobufInputParser extends InputParser[ProtobufInput] with Logging {
    
  def parse(input: ProtobufInput) : DataInput = {

      // Load the weights
      log.info(s"Parsing weights from '${input.inputWeightsPath}'")
      val weightInputStream = new FileInputStream(input.inputWeightsPath)
      val weightsIter = Iterator.continually {
        FactorGraphProtos.Weight.parseDelimitedFrom(weightInputStream)
      }.takeWhile(_ != null)
      val weightsMap = weightsIter.map { weight =>
        (weight.getId.toInt, Weight(weight.getId.toInt, weight.getInitialValue, weight.getIsFixed))
      }.toMap
      weightInputStream.close()

      // Load the variables
      log.info(s"Parsing variables from '${input.inputVariablesPath}'")
      val variablesInputStream = new FileInputStream(input.inputVariablesPath)
      val variablesIterator = Iterator.continually {
        FactorGraphProtos.Variable.parseDelimitedFrom(variablesInputStream)
      }.takeWhile(_ != null)
      val variablesMap = variablesIterator.map { variable =>
        (variable.getId.toInt, BooleanVariable(
          variable.getId.toInt, 
          Option(variable.getInitialValue).getOrElse(0.0), 
          (variable.hasInitialValue),
          (!variable.hasInitialValue)))
      }.toMap
      variablesInputStream.close()

      // Load the factors
      log.info(s"Parsing factors from '${input.inputFactorsPath}'")
      val factorsInputStream = new FileInputStream(input.inputFactorsPath)
      val factorsIterator = Iterator.continually {
        FactorGraphProtos.Factor.parseDelimitedFrom(factorsInputStream)
      }.takeWhile(_ != null)
      val factorMap = scala.collection.mutable.Map[Int, Factor](factorsIterator.map { factor =>
        val factorFunction = factor.getFactorFunction match {
          case FactorGraphProtos.Factor.FactorFunctionType.IMPLY => ImplyFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.OR => OrFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.AND => AndFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.EQUAL => EqualFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.ISTRUE => IsTrueFactorFunction
        }
        (factor.getId.toInt, Factor(factor.getId.toInt, Nil, factor.getWeightId.toInt, factorFunction))
      }.toSeq: _*)
      factorsInputStream.close()

      // Load the edges. TODO: The grouping is very inefficient
      log.info(s"Parsing edges from '${input.inputEdgesPath}'")
      val edgesInputStream = new FileInputStream(input.inputEdgesPath)
      val edgesIterator = Iterator.continually {
        FactorGraphProtos.GraphEdge.parseDelimitedFrom(edgesInputStream)
      }.takeWhile(_ != null)
      val groupedEdges = edgesIterator.toList.groupBy(_.getFactorId).mapValues(_.sortBy(_.getPosition))
      groupedEdges.foreach { case (factorId, edges) =>
        val factorVars = edges.map { edge =>
          val variableId = edge.getVariableId
          val isPositive = edge.getIsPositive
          FactorVariable(variableId.toInt, isPositive)
        }.toList
        factorMap(factorId.toInt) = factorMap(factorId.toInt).copy(variables = factorVars)
      }
      edgesInputStream.close()

     DataInput(weightsMap, variablesMap, factorMap.toMap)
  }


}

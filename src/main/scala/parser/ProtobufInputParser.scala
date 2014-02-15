package org.dennybritz.sampler

import scala.io.Source
import scala.collection.mutable.{Map => MMap}
import scala.util.matching._
import org.deepdive.serialization.FactorGraphProtos
import scala.collection.JavaConversions._

case class ProtobufInput(graphFile: String)
//case class VariableFactorMap(variables: Map[Int, _ <: Variable], factorMap: Map[Int, List[FactorVariable]])

object ProtobufInputParser extends InputParser[ProtobufInput] with Logging {
    
  def parse(input: ProtobufInput) : DataInput = {

      // Load the graph
      val graph = FactorGraphProtos.FactorGraph.newBuilder
      graph.mergeFrom(new java.io.FileInputStream(input.graphFile))
      
      val weightsMap = graph.getWeightList.map { weight =>
        (weight.getId.toInt, Weight(weight.getId.toInt, weight.getInitialValue, weight.getIsFixed))
      }.toMap

      val variablesMap = graph.getVariableList.map { variable =>
        (variable.getId.toInt, BooleanVariable(
          variable.getId.toInt, 
          Option(variable.getInitialValue).getOrElse(0.0), 
          (variable.hasInitialValue),
          (!variable.hasInitialValue)))
      }.toMap

      val factorMap = scala.collection.mutable.Map[Int, Factor](graph.getFactorList.map { factor =>
        val factorFunction = factor.getFactorFunction match {
          case FactorGraphProtos.Factor.FactorFunctionType.IMPLY => ImplyFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.OR => OrFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.AND => AndFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.EQUAL => EqualFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.ISTRUE => IsTrueFactorFunction
        }
        (factor.getId.toInt, Factor(factor.getId.toInt, Nil, factor.getWeightId.toInt, factorFunction))
      }.toSeq: _*)

      // Very inefficient, TODO
      val groupedEdges = graph.getEdgeList.groupBy(_.getFactorId).mapValues(_.sortBy(_.getPosition))
      groupedEdges.foreach { case (factorId, edges) =>
        val factorVars = edges.map { edge =>
          val variableId = edge.getVariableId
          val isPositive = edge.getIsPositive
          FactorVariable(variableId.toInt, isPositive)
        }.toList
        factorMap(factorId.toInt) = factorMap(factorId.toInt).copy(variables = factorVars)
      }

     DataInput(weightsMap, variablesMap, factorMap.toMap)
  }


}

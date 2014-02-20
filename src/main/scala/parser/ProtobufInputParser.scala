package org.dennybritz.sampler

import java.io.FileInputStream
import scala.io.Source
import scala.collection.mutable.{Map => MMap, ArrayBuffer}
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
      val weights = weightsIter.map { weight =>
        Weight(weight.getId.toInt, weight.getInitialValue, weight.getIsFixed)
      }.toVector.sortBy(_.id)
      weightInputStream.close()
      // Assert that we have sequential ids for indexing
      weights.iterator.map(_.id).zipWithIndex.find(t => t._1 != t._2).foreach { case(idx, weightId) =>
        throw new IllegalArgumentException(s"Weight ${weightId} did not match id=${idx}")
      }


      // Load the variables
      log.info(s"Parsing variables from '${input.inputVariablesPath}'")
      val variablesInputStream = new FileInputStream(input.inputVariablesPath)
      val variablesIterator = Iterator.continually {
        FactorGraphProtos.Variable.parseDelimitedFrom(variablesInputStream)
      }.takeWhile(_ != null)
      val variables = ArrayBuffer(variablesIterator.map { variable =>
        BooleanVariable(
          variable.getId.toInt, 
          Option(variable.getInitialValue).getOrElse(0.0), 
          (variable.hasInitialValue),
          (!variable.hasInitialValue), Nil)
      }.toSeq: _*).sortBy(_.id)
      variablesInputStream.close()
      variables.iterator.map(_.id).zipWithIndex.find(t => t._1 != t._2).foreach { case(idx, variableId) =>
        throw new IllegalArgumentException(s"Variable ${variableId} did not match id=${idx}")
      }

      // Load the factors
      log.info(s"Parsing factors from '${input.inputFactorsPath}'")
      val factorsInputStream = new FileInputStream(input.inputFactorsPath)
      val factorsIterator = Iterator.continually {
        FactorGraphProtos.Factor.parseDelimitedFrom(factorsInputStream)
      }.takeWhile(_ != null)
      val factors = ArrayBuffer(factorsIterator.map { factor =>
        val factorFunction = factor.getFactorFunction match {
          case FactorGraphProtos.Factor.FactorFunctionType.IMPLY => ImplyFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.OR => OrFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.AND => AndFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.EQUAL => EqualFactorFunction
          case FactorGraphProtos.Factor.FactorFunctionType.ISTRUE => IsTrueFactorFunction
        }
        Factor(factor.getId.toInt, Nil, factor.getWeightId.toInt, factorFunction)
      }.toSeq: _*).sortBy(_.id)
      factorsInputStream.close()
      factors.iterator.map(_.id).zipWithIndex.find(t => t._1 != t._2).foreach { case(idx, factorId) =>
        throw new IllegalArgumentException(s"Factor ${factorId} did not match id=${idx}")
      }

      // Load the edges. TODO: The grouping is very inefficient
      log.info(s"Parsing edges from '${input.inputEdgesPath}'")
      val edgesInputStream = new FileInputStream(input.inputEdgesPath)
      val edgesIterator = Iterator.continually {
        FactorGraphProtos.GraphEdge.parseDelimitedFrom(edgesInputStream)
      }.takeWhile(_ != null)
      edgesIterator.foreach { edge =>
        val factorId = edge.getFactorId
        val variableId = edge.getVariableId
        val isPositive = edge.getIsPositive
        val position = edge.getPosition

        val oldVar = variables(variableId.toInt)
        variables(variableId.toInt) = oldVar.copy(factorIds = oldVar.factorIds :+ factorId.toInt)
        val oldFactor = factors(factorId.toInt)
        val factorVar = FactorVariable(variableId.toInt, isPositive, position.toInt)
        factors(factorId.toInt) = oldFactor.copy(variables= (oldFactor.variables :+ factorVar).sortBy(_.position))
      }
      edgesInputStream.close()

     DataInput(weights, variables.toVector, factors.toVector)
  }


}

package org.dennybritz.sampler

import scala.io.Source
import scala.collection.mutable.{Map => MMap}
import scala.util.matching._

case class DeepDiveInput(factorsFilePath: String, variablesFilePath: String, weightsFilePath: String)
case class VariableFactorMap(variables: Set[_ <: Variable], factorMap: Map[Int, List[FactorVariable]])

object DeepDiveInputParser extends InputParser[DeepDiveInput] with Logging {
    
  val splitRegex = """\t""".r

  def parse(input: DeepDiveInput) : DataInput = {
     val weightsMap = parseWeights(input.weightsFilePath).map (w => (w.id, w)).toMap
     val variableFactorMap = parseVariableFactorMap(input.variablesFilePath)
     val factors = parseFactors(input.factorsFilePath, variableFactorMap.factorMap)
     val factorMap = factors.map (f => (f.id, f)).toMap
     val variablesMap = variableFactorMap.variables.map(v => (v.id, v)).toMap
     DataInput(weightsMap, variablesMap, factorMap)
  }

  def parseWeights(weightsFilePath: String) : List[Weight] = {
    Source.fromFile(weightsFilePath).getLines.map(l => splitRegex.split(l)).zipWithIndex.map {
      case (Array(weightId, initialValue, isFixed, description), _) =>
        Weight(weightId.toInt, initialValue.toDouble, isFixed.toBoolean)
      case (obj, index) =>
        throw new RuntimeException(s"Could not parse weights file at line ${index}: ${obj.mkString(" ")}")
    }.toList
  } 

  def parseVariableFactorMap(variableFilePath: String) : VariableFactorMap = {

    // Temporary case class for parsing rows
    case class Row(variableId: Int, factorId: Int, position: Int, isPositive: Boolean, 
      dataType: String, initialValue: Double, isEvidence: Boolean, isQuery: Boolean)

    // Read all rows
    val rows = Source.fromFile(variableFilePath).getLines.map(l => splitRegex.split(l)).zipWithIndex.map {
      case(Array(variableId, factorId, position, isPositive, dataType, initialValue, 
        isEvidence, isQuery), _) =>
        Row(variableId.toInt, factorId.toInt, position.toInt, isPositive.toBoolean,
          dataType, initialValue.toDouble, isEvidence.toBoolean, isQuery.toBoolean)
      case(obj, index) =>
        throw new RuntimeException(s"Could not parse weights file at line ${index}: ${obj}")
    }.toList

    // Build a list of variables
    val variableList = rows map { row => 
      BooleanVariable(row.variableId, row.initialValue, row.isEvidence, row.isQuery)
    } toSet

    // Build a map from factorID -> variable
    val factorMap = rows.map { row =>
      (row.factorId, FactorVariable(row.variableId, row.isPositive))
    }.toList.groupBy(_._1).mapValues(_.map(_._2))

    VariableFactorMap(variableList, factorMap.toMap)
  }

  def parseFactors(factorsFile: String, variableMap: Map[Int, List[FactorVariable]]) = {
    Source.fromFile(factorsFile).getLines.map(l => splitRegex.split(l)).zipWithIndex.map {
      case (Array(factorId, weightId, "ImplyFactorFunction"), _) =>
        Factor(factorId.toInt, variableMap.get(factorId.toInt).getOrElse(Nil), weightId.toInt, ImplyFactorFunction)
      case (obj, index) =>
        throw new RuntimeException(s"Could not parse factors file at line ${index}: ${obj}")
    }.filterNot(_.variables.size == 0).toList
  }

}

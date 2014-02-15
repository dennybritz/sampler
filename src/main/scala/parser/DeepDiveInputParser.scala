package org.dennybritz.sampler

import scala.io.Source
import scala.collection.mutable.{Map => MMap}
import scala.util.matching._

case class DeepDiveInput(factorsFilePath: String, variablesFilePath: String, weightsFilePath: String)
case class VariableFactorMap(variables: Map[Int, _ <: Variable], factorMap: Map[Int, List[FactorVariable]])

object DeepDiveInputParser extends InputParser[DeepDiveInput] with Logging {
    
  val splitRegex = """\t""".r

  def parse(input: DeepDiveInput) : DataInput = {
     val weightsMap = parseWeights(input.weightsFilePath)
     val variableFactorMap = parseVariableFactorMap(input.variablesFilePath)
     val factorMap = parseFactors(input.factorsFilePath, variableFactorMap.factorMap)
     val variablesMap = variableFactorMap.variables
     DataInput(weightsMap, variablesMap, factorMap)
  }

  def parseWeights(weightsFilePath: String) : Map[Int, Weight] = {
    log.debug("Parsing weights...")
    Source.fromFile(weightsFilePath).getLines.map(l => splitRegex.split(l)).zipWithIndex.map {
      case (Array(weightId, initialValue, isFixed, description), _) =>
        Tuple2(weightId.toInt, Weight(weightId.toInt, initialValue.toDouble, isFixed.toBoolean))
      case (obj, index) =>
        throw new RuntimeException(s"Could not parse weights file at line ${index}: ${obj.mkString(" ")}")
    }.toMap
  } 

  def parseVariableFactorMap(variableFilePath: String) : VariableFactorMap = {

    // Temporary case class for parsing rows
    case class Row(variableId: Int, factorId: Int, position: Int, isPositive: Boolean, 
      dataType: String, initialValue: Double, isEvidence: Boolean, isQuery: Boolean)

    // Read all rows
    log.debug("Mapping rows...")
    val linesIterator = Source.fromFile(variableFilePath).getLines
    val rowsIterator = linesIterator.map(l => splitRegex.split(l)).zipWithIndex.map {
      case(Array(variableId, factorId, position, isPositive, dataType, initialValue, 
        isEvidence, isQuery), _) =>
        Row(variableId.toInt, factorId.toInt, position.toInt, isPositive.toBoolean,
          dataType, initialValue.toDouble, isEvidence.toBoolean, isQuery.toBoolean)
      case(obj, index) =>
        throw new RuntimeException(s"Could not parse weights file at line ${index}: ${obj}")
    }

    // Build a list of variables
    log.debug("Generating variables and factor list...")
    val parsedRows = rowsIterator map { row => 
      val variableTuple = Tuple2(row.variableId, 
        BooleanVariable(row.variableId, row.initialValue, row.isEvidence, row.isQuery))
      val factorTuple = Tuple3(row.factorId, row.position, FactorVariable(row.variableId, row.isPositive))
      Tuple2(variableTuple, factorTuple)
    } 

    log.debug("Unzipping variables and factors...")
    val (variableTuples, factorTuples) = parsedRows.toList.unzip

    // Build a map from factorID -> variable
    log.debug("Building factor map...")
    val factorMap = factorTuples.groupBy(_._1).mapValues(_.sortBy(_._2).map(_._3))

    log.debug("Building variable map...")
    val variableMap = variableTuples.toMap

    VariableFactorMap(variableMap, factorMap)
  }

  def parseFactors(factorsFile: String, variableMap: Map[Int, List[FactorVariable]]) = {
    log.debug("Parsing factors...")
    Source.fromFile(factorsFile).getLines.map(l => splitRegex.split(l)).zipWithIndex.map {
      case (Array(factorId, weightId, functionType), _) =>
        val funcObj = functionType match {
          case "ImplyFactorFunction" => ImplyFactorFunction
          case "OrFactorFunction" => OrFactorFunction
          case "AndFactorFunction" => AndFactorFunction
          case "EqualFactorFunction" => EqualFactorFunction
          case "IsTrueFactorFunction" => IsTrueFactorFunction
        }
        (factorId.toInt, Factor(factorId.toInt, 
          variableMap.get(factorId.toInt).getOrElse(Nil), weightId.toInt, funcObj))
      case (obj, index) =>
        throw new RuntimeException(s"Could not parse factors file at line ${index}: ${obj}")
    }.filterNot(_._2.variables.size == 0).toMap
  }

}

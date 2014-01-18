package org.dennybritz.sampler

import java.io.File

case class Config(weightsFile: File, factorsFile: File, variablesFile: File)

object Runner extends App with Logging {

  val parser = new scopt.OptionParser[Config]("scopt") {
    opt[File]('w', "weights") required() valueName("<weightsFile>") action { (x, c) =>
      c.copy(weightsFile = x) } text("weights File")
    opt[File]('v', "variables") required() valueName("<variablesFile>") action { (x, c) =>
      c.copy(variablesFile = x) } text("variables File")
    opt[File]('f', "factors") required() valueName("<factorsFile>") action { (x, c) =>
      c.copy(factorsFile = x) } text("factors File")
  }

  val config = parser.parse(args, Config(null, null, null)).getOrElse{
    System.exit(1)
    throw new RuntimeException("")
  }
  
  val parserInput = DeepDiveInput(config.factorsFile.getCanonicalPath, config.variablesFile.getCanonicalPath,
    config.weightsFile.getCanonicalPath)
  
  val dataInput = DeepDiveInputParser.parse(parserInput)
  val graphContext = GraphContext.create(dataInput)
  val learner = new Learner(graphContext)
  learner.learnWeights(100, 1, 0.1, 0.01, 0.96)

}
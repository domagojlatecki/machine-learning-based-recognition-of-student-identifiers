package at.doml.thesis.training

sealed abstract class Error(msg: String) extends RuntimeException(msg) with Product with Serializable {
  override def toString: String = msg
}

object Error {
  type ![+A] = Either[Error, A]
}

final case class UnknownCommandError(cmd: String) extends Error(s"Unknown command: $cmd")

final case class UnknownArgumentError(arg: String) extends Error(s"Unknown argument: $arg")

final case class NumberParseError(arg: String) extends Error(s"Cannot parse argument as required number type: $arg")

final case class IllegalArgumentError(arg: String) extends Error(s"Illegal argument provided: $arg")

final case class MissingArgumentError(arg: String) extends Error(s"Missing required argument(s): $arg")

final case class NoArgumentsProvidedError(help: String) extends Error("No arguments were provided")

final case class InvalidFileLabelsError(file: String) extends Error(s"Missing or invalid labels for file: $file")

final case class NoFilesProvidedError(dir: String) extends Error(s"No files found in directory: $dir")

final case class NoDataProvidedError(dir: String) extends Error(s"No sample data found in directory: $dir")

final case class CannotLoadFileError(file: String) extends Error(s"Cannot load file: $file")

final case class CannotWriteFileError(file: String) extends Error(s"Cannot write to file: $file")

final case class ParseError(file: String, line: String) extends Error(s"Error in file: $file, cannot parse line: $line")

final case class EmptyFileError(file: String) extends Error(s"No layers found for neural network in file: $file")

final case class InvalidLayerOutputDimension(dim: Int) extends Error(s"Invalid layer output dimension: $dim")

final case class InvalidLayerInputDimension(dim: Int) extends Error(s"Invalid layer input dimension: $dim")

final case class InvalidNumberOfNeurons(num: Int) extends Error(s"Invalid number of neurons: $num")

final case class InvalidNeuronWeights(weights: String) extends Error(s"Invalid neuron weights: $weights")

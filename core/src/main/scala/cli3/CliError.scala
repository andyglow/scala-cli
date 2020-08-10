package cli3


sealed trait CliError

sealed trait TokenizeError extends CliError
object TokenizeError {
  final case object UnexpectedEOL extends TokenizeError
}

sealed trait RecognizeError extends CliError
object RecognizeError {
  final case object EmptyString extends RecognizeError
  final case class IllegalFirstChar(char: Char) extends RecognizeError
  final case class IllegalLastChar(char: Char) extends RecognizeError
  final case class UnknownProperty(name: String) extends RecognizeError
  final case class FlagWithValue(flag: Def.Flag, value: String) extends RecognizeError
  final case class OptionWithoutValue(flag: Def.Opt) extends RecognizeError
  final case class IllegalSyntax(text: String) extends RecognizeError
  final case class UnknownIndexedProp(text: String) extends RecognizeError
  final case class UnquotedString(text: String) extends RecognizeError
  final case class SubError(cmd: Def.Cmd, error: RecognizeError) extends RecognizeError
  final case class MissingRequiredProps(props: Seq[String]) extends RecognizeError

  final case class Custom(text: String) extends RecognizeError
  final case class FormatError(text: String) extends RecognizeError
  final case class RepetitionOfNonRepetitive(flag: Def.Keyed) extends RecognizeError
  final case class UnexpectedFlag(flag: Def.Flag) extends RecognizeError
  final case class UnexpectedOpt(opt: Def.Opt, value: String) extends RecognizeError
  final case class UnexpectedArg(arg: Def.Arg, value: String) extends RecognizeError
  final case class ArgAlreadySet(arg: Def.Arg, value: String) extends RecognizeError
  final case class IllegalState(state: Any) extends RecognizeError
  final case class UnknownCmd(cmd: Def.Cmd) extends RecognizeError

}
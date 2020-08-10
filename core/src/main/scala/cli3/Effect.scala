package cli3


sealed trait Effect[+T] {
  def isOk: Boolean
  final def isErr: Boolean = !isOk
  def map[R](f: T => R): Effect[R]
  def flatMap[R](f: T => Effect[R]): Effect[R]
  def getOrElse[TT >: T](alt: => TT): TT
}

final case class Ok[T](value: T) extends Effect[T] {
  def isOk: Boolean = true
  def map[R](f: T => R): Effect[R] = Ok(f(value))
  def flatMap[R](f: T => Effect[R]): Effect[R] = f(value)
  def getOrElse[TT >: T](alt: => TT): TT = value
}

sealed trait Err extends Effect[Nothing] {
  final def isOk: Boolean = false
  final def map[R](f: Nothing => R): Effect[R] = this.asInstanceOf[Effect[R]]
  final def flatMap[R](f: Nothing => Effect[R]): Effect[R] = this.asInstanceOf[Effect[R]]
  def getOrElse[TT >: Nothing](alt: => TT): TT = alt
}

sealed trait TokenizeErr extends Err

object TokenizeErr {
  final case object UnexpectedEOL extends TokenizeErr
}

sealed trait RecognizeErr extends Err

object RecognizeErr {
  final case object EmptyString extends RecognizeErr
  final case class IllegalFirstChar(char: Char) extends RecognizeErr
  final case class IllegalLastChar(char: Char) extends RecognizeErr
  final case class UnknownProperty(name: String) extends RecognizeErr
  final case class FlagWithValue(flag: Def.Flag, value: String) extends RecognizeErr
  final case class OptionWithoutValue(flag: Def.Opt) extends RecognizeErr
  final case class IllegalSyntax(text: String) extends RecognizeErr
  final case class UnknownIndexedProp(text: String) extends RecognizeErr
  final case class UnquotedString(text: String) extends RecognizeErr
  final case class SubError(cmd: Def.Cmd, error: Err) extends RecognizeErr
  final case class MissingRequiredProps(props: Seq[String]) extends RecognizeErr

  final case class Custom(text: String) extends RecognizeErr
  final case class FormatError(text: String) extends RecognizeErr
  final case class RepetitionOfNonRepetitive(flag: Def.Keyed) extends RecognizeErr
  final case class UnexpectedFlag(flag: Def.Flag) extends RecognizeErr
  final case class UnexpectedOpt(opt: Def.Opt, value: String) extends RecognizeErr
  final case class UnexpectedArg(arg: Def.Arg, value: String) extends RecognizeErr
  final case class ArgAlreadySet(arg: Def.Arg, value: String) extends RecognizeErr
  final case class IllegalState(state: Any) extends RecognizeErr
  final case class UnknownCmd(cmd: Def.Cmd) extends RecognizeErr
}

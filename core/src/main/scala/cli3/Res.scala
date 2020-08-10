package cli3

import cli3. { Def => D }

object Res {

  /* +----------
   * | Args
   * +------------------
   */
  final case class Args private (
    expectedArgCount: Int,
    hasVararg: Boolean,
    minVarArgCount: Int,
    ones: Vector[String] = Vector.empty,
    last: Option[Vector[String]] = None) {

    def updated(a: D.Arg, v: String): Args = {
      a match {
        case _: D.Arg.Arg1   => copy(ones = ones :+ v)
        case _: D.Arg.VarArg => copy(last = last match {
          case None        => Some(Vector(v))
          case Some(items) => Some(items :+ v)
        })
      }
    }

    def get(i: Int): Either[String, Option[String]] = {
      if (i < 0) Left("out of bounds")
      else if (i < expectedArgCount) Right(ones unapply i)
      else if (hasVararg) Right(last.flatMap(_.unapply(i - expectedArgCount)))
      else Left("out of bounds")
    }

    def list: Vector[String] = ones ++ last.getOrElse(Vector.empty)

    def length: Int = ones.length + last.map(_.length).getOrElse(0)
  }

  object Args {

    def apply(cmd: D.Cmd): Args = cmd.indexedProps match {
      case args: D.Args.Growable => Args(args.elements.length, hasVararg = false, 0)
      case args: D.Args.Final    => Args(args.ones.length, hasVararg = true, args.last.minOcc)
      case _                   => Args(0, hasVararg = false, 0)
    }
  }

  /* +----------
   * | Flags
   * +------------------
   */
  final case class Flags private (elements: Map[D.Flag, Int] = Map.empty) {

    def isEmpty: Boolean = elements.isEmpty

    def nonEmpty: Boolean = !isEmpty

    def set(f: D.Flag): Flags = copy(elements = elements.updatedWith(f) { _.map(_ + 1) orElse Some(0) })

    def unset(f: D.Flag): Flags = copy(elements = elements.updatedWith(f) { _.map(_ - 1) orElse None })

    def isSet(k: Key): Boolean = num(k) > 0

    def num(k: Key): Int = find(k).map { case (_, n) => n } getOrElse 0

    def find(k: Key): Option[(D.Flag, Int)] = elements collectFirst {
      case (o, v) if o.key =:= k => (o, v)
    }

    def get(k: Key): Option[Int] = find(k) map { case (_, v) => v }
  }

  object Flags {

    def apply(cmd: D.Cmd): Flags = Flags(cmd.keyedProps.elements.collect { case x: D.Flag => x }.map { (_, 0 ) }.toMap )
  }

  /* +----------
   * | Opts
   * +------------------
   */
  final case class Opts private (elements: Map[D.Opt, List[String]] = Map.empty) {

    def isEmpty: Boolean = elements.isEmpty

    def nonEmpty: Boolean = !isEmpty

    def updated(o: D.Opt, v: String): Opts = {
      def updated = elements.updatedWith(o) {
        _.map(_ :+ v) orElse Some(List(v))
      }

      copy(elements = updated)
    }

    def find(k: Key): Option[(D.Opt, List[String])] = elements collectFirst {
      case (o, v) if o.key =:= k => (o, v)
    }

    def get(k: Key): Option[List[String]] = find(k) map { case (_, opt) => opt }
  }

  object Opts {

    def apply(cmd: D.Cmd): Opts = new Opts(cmd.keyedProps.elements.collect { case x: D.Opt => x }.map { (_, Nil ) }.toMap)
  }

  /* +----------
   * | Cmds
   * +------------------
   */
  final case class Cmd(
    flags: Flags,
    opts: Opts,
    args: Args,
    cmd: Option[(D.Cmd, Cmd)]) {

    def withFlag(f: D.Flag): Cmd = copy(flags = flags.set(f))

    def withOpt(o: D.Opt, v: String): Cmd = copy(opts = opts.updated(o, v))

    def withArg(o: D.Arg, v: String): Cmd = copy(args = args.updated(o, v))

    def withCmd(c: D.Cmd, v: Cmd): Cmd = copy(cmd = Some(c -> v))
  }

  object Cmd {

    def apply(cmd: D.Cmd): Cmd = Cmd(
      Flags(cmd),
      Opts(cmd),
      Args(cmd),
      None)
  }

  private object SubAdapter extends SubCommandAdapter[Cmd] {
    override type Command = Cmd
    override def init(cmd: Def.Cmd): Either[RecognizeError, (Cmd, cli3.Builder[Cmd])] = Right(Cmd(cmd), new Builder)
  }

  class Builder extends cli3.Builder[Cmd]()(SubAdapter) {
    override def withFlag(init: Cmd, flag: Def.Flag): Either[RecognizeError, Cmd] = Right(init.withFlag(flag))
    override def withOpt(init: Cmd, opt: Def.Opt, value: String): Either[RecognizeError, Cmd] = Right(init.withOpt(opt, value))
    override def withArg(init: Cmd, arg: Def.Arg, value: String): Either[RecognizeError, Cmd] = Right(init.withArg(arg, value))
    override def withSubCmd(init: Cmd, cmd: Def.Cmd, value: sub.Command): Either[RecognizeError, Cmd] = Right(init.withCmd(cmd, value.asInstanceOf[Cmd]))
  }
}

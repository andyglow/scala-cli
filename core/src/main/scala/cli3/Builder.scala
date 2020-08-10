package cli3

import cli3.Builder.UnitBuilder
import cli3.{Def => D}


trait SubCommandAdapter[T] {
  type Command
  def init(cmd: D.Cmd): Effect[(Command, Builder[Command])]
}

abstract class Builder[T](implicit val sub: SubCommandAdapter[T]) {
  def withFlag(init: T, flag: D.Flag): Effect[T]
  def withOpt(init: T, opt: D.Opt, value: String): Effect[T]
  def withArg(init: T, opt: D.Arg, value: String): Effect[T]
  def init(cmd: D.Cmd): Effect[(sub.Command, Builder[sub.Command])] = sub.init(cmd)
  def withSubCmd(init: T, cmd: D.Cmd, value: sub.Command): Effect[T]
}

object Builder {
  object UnitBuilder extends Builder[Unit]()(new UnitAdapter[Unit]()) {
    override def withFlag(init: Unit, flag: Def.Flag): Effect[Unit] = Ok(init)
    override def withOpt(init: Unit, opt: Def.Opt, value: String): Effect[Unit] = Ok(init)
    override def withArg(init: Unit, arg: Def.Arg, value: String): Effect[Unit] = Ok(init)
    override def withSubCmd(init: Unit, cmd: Def.Cmd, value: sub.Command): Effect[Unit] = Ok(init)
  }
  class UnitAdapter[T] extends SubCommandAdapter[T] {
    type Command = Unit
    def init(cmd: D.Cmd): Effect[(Unit, Builder[Unit])] = Ok(((), UnitBuilder))
  }
  abstract class NoCmd[T] extends Builder[T]()(new UnitAdapter[T]) {
    final override def withSubCmd(init: T, cmd: Def.Cmd, value: sub.Command): Effect[T] = Ok(init)
  }
}
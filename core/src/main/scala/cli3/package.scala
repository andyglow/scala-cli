import cli3.Def._

import scala.language.implicitConversions

package object cli3 {

  implicit def argToArgs(x: Arg.Arg1): Args = Args() :+ x
  implicit def varArgToArgs(x: Arg.VarArg): Args = Args() :+ x
  implicit def keyedToKeyedProps(x: Keyed): KeyedProps = KeyedProps(List(x))
  implicit def cmdToCmds(x: Cmd): Cmds = Cmds(List(x))

  implicit def charToLabel(x: Char): Key = Key.Short(x)
  implicit def stringToLabel(x: String): Key = Key.Long(x)

  implicit class _3CharOps(private val x: Char) extends AnyVal {
    def |(y: String): Key.Full = Key.Full(x, y)
    private[cli3] def isQ: Boolean = x == '\'' || x == '"' || x == '`'
  }
}

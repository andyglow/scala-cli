package cli3

import cli3. { Def => D }
import java.io.PrintStream

object PrintUsage {

  sealed trait Mode
  final object Mode {
    final case object OneColumn extends Mode
    final case object TwoColumns extends Mode
  }

  trait Renderer {
    // TODO
    //   - add other features like 'head', 'opt', 'flag', etc
    def printTitle(title: String, version: Option[String], ps: PrintStream): Unit
  }
  final object Renderer {

    final case object Standard extends Renderer {
      def printTitle(title: String, version: Option[String], ps: PrintStream): Unit = {
        ps.print(title)
        version foreach { version =>
          ps.print(" v")
          ps.print(version)
        }
      }
    }

    // TODO
    //   - release colored renderer as separate module leveraging jansi
    //   - allow customisable color scheme
  }
}

case class PrintUsage(
  printer: PrintUsage.Renderer = PrintUsage.Renderer.Standard,
  mode: PrintUsage.Mode = PrintUsage.Mode.OneColumn) {

  def apply(defn: D.Cmd, ps: PrintStream): Unit = {
    internal(defn, 0, ps)
  }

  private def internal(defn: D.Cmd, indent: Int, ps: PrintStream): Unit = {
    import defn._

    val space = "  " * indent
    def printIndent(): Unit = ps.print(space)

    if (indent == 0)
      title foreach { title =>
        printer.printTitle(title, version, ps)
        ps.println()
      }

    header foreach { h =>
      h.linesIterator foreach { l =>
        printIndent()
        ps.println(l)
      }
    }

    printIndent()
    ps.print("Usage: ")
    ps.print(program)
    if (opts.nonEmpty || flags.nonEmpty) ps.print(" [options]")
    if (cmds.nonEmpty) if (indent == 0) ps.print(" [command]") else ps.print(" [sub-command]")
    args foreach {
      case D.Arg.Arg1(l, _)   => ps.print(s" <$l>")
      case D.Arg.VarArg(l, _) => ps.print(s" <${l}1> ... <${l}N>")
    }
    ps.println()

    flags foreach { case D.Flag(l) =>
      printIndent()
      ps.print(l.toString)
      ps.println()
    }

    opts foreach { case D.Opt(l, r, vl, t) =>
      printIndent()
      ps.print(l.toString)
      ps.print("=")
      ps.print(s"<$vl>")
      if (r) ps.print(" required")
      t foreach { x => ps.print(s". $x") }
      ps.println()
    }

    args foreach {
      case D.Arg.Arg1(l, r) =>
        printIndent()
        ps.print(s"<$l>")
        if (r) ps.print(" required")
        ps.println()
      case D.Arg.VarArg(l, minOcc) =>
        printIndent()
        ps.print(s"<${l}1> ... <${l}N>")
        if (minOcc > 0) ps.print(s" minimum occurrence $minOcc")
        ps.println()
    }

    if (cmds.nonEmpty) {
      ps.println()
      printIndent()
      if (indent == 0)
        ps.println("Commands:")
      else
        ps.println("Sub-Commands:")
    }

    cmds foreach { cmd =>
      printIndent()
      ps.println(cmd.program)
      internal(cmd, indent + 1, ps)
      ps.println()
    }

    footer foreach { f =>
      f.linesIterator foreach { l =>
        printIndent()
        ps.println(l)
      }
    }
  }
}

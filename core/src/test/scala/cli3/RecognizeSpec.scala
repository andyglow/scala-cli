package cli3

import java.nio.file.{Files, Path, Paths}

import org.scalatest.matchers.should.Matchers._
import org.scalatest.EitherValues._
import org.scalatest.funsuite._
import ResultedCmdMatchers._
import EffectValues._
import cli3.{Res => R}
import cli3.Def._
import cli3.RecognizeSpec.FsApp
import org.scalatest.BeforeAndAfterAll


class RecognizeSpec extends AnyFunSuite {

  private val tempFile = {
    val path = Files.createTempFile("cli3-recognize", "spec")
    path.toFile.deleteOnExit()
    path
  }
  // keys come with heading dashes already stripped
  // -a    -> a
  // --abc -> abc
  test("key") {
    Recognize.key("abc").value shouldBe "abc"
    Recognize.key("abc.def").value shouldBe "abc.def"
    Recognize.key("abc-def").value shouldBe "abc-def"
    Recognize.key("abc_def").value shouldBe "abc_def"
    Recognize.key("abc def").value shouldBe "abc"
    Recognize.key("abc=def").value shouldBe "abc"
    Recognize.key("abc/def").value shouldBe "abc"
    Recognize.key("_abc") shouldBe RecognizeErr.IllegalFirstChar('_')
    Recognize.key(":abc") shouldBe RecognizeErr.IllegalFirstChar(':')
    Recognize.key("-abc") shouldBe RecognizeErr.IllegalFirstChar('-')
    Recognize.key("abc-") shouldBe RecognizeErr.IllegalLastChar('-')
    Recognize.key("abc.") shouldBe RecognizeErr.IllegalLastChar('.')
    Recognize.key("abc_") shouldBe RecognizeErr.IllegalLastChar('_')
  }

  test("keyed") {
    val cmd = new Cmd("program", keyedProps = Flag("abc") :+ Opt("def"))

    Recognize.keyed("abc", cmd).value shouldBe (Flag("abc"), None)
    Recognize.keyed("abc xxx", cmd) shouldBe RecognizeErr.FlagWithValue(Flag("abc"), "xxx")

    Recognize.keyed("def", cmd).value shouldBe (Opt("def"), None)
    Recognize.keyed("def xxx", cmd).value shouldBe (Opt("def"), Some("xxx"))
    Recognize.keyed("def=xxx", cmd).value shouldBe (Opt("def"), Some("xxx"))
  }

  test("cmd. flags + props") {
    implicit val cmd = new Cmd(
      "program",
      keyedProps = Flag("abc") :+ Flag('v') :+
                   Opt("def").! :+ Opt('m'|"mode"))

    // simple equal based equation
    Recognize.ast("--abc", "--def=xxx").value should have (
      flag.set("abc"),
      flag.unset('v'),
      opt.set("def", "xxx")
    )

    // simple space based equation
    Recognize.ast("--abc", "--def xxx").value should have (
      flag.set("abc"),
      flag.unset('v'),
      opt.set("def", "xxx")
    )

    // simple equation with value coming later
    Recognize.ast("--abc", "--def", "xxx").value should have (
      flag.set("abc"),
      flag.unset('v'),
      opt.set("def", "xxx")
    )

    // detect short opt notation
    Recognize.ast("--def", "xxx", "-m", "yyy").value should have (
      flag.unset('v'),
      opt.set("def", "xxx"),
      opt.set('m', "yyy"),
      opt.set("mode", "yyy")
    )

    // detect long opt notation
    Recognize.ast("--def", "xxx", "--mode", "yyy").value should have (
      flag.unset('v'),
      opt.set("def", "xxx"),
      opt.set('m', "yyy"),
      opt.set("mode", "yyy")
    )

    // simple equation with no value coming later nor specified in-place
    Recognize.ast("--abc", "--def") shouldBe RecognizeErr.OptionWithoutValue(Opt("def").!)

    // missed required option
    Recognize.ast("--abc") shouldBe RecognizeErr.MissingRequiredProps(Seq("--def"))
  }

  test("cmd. args. basic") {
    // 0-req, 1-opt
    implicit val cmd = new Cmd(
      "program",
      indexedProps = Arg("0") :+ Arg("1"))

    // both specified
    Recognize.ast("000", "111").value should have (
      arg.set(0, "000"),
      arg.set(1, "111")
    )

    // only 0 specified
    Recognize.ast("000").value should have (
      arg.set(0, "000"),
      arg.unset(1)
    )

    // nothing
    Recognize.ast(Array.empty[String]) shouldBe RecognizeErr.MissingRequiredProps(Seq("<0>"))
  }

  test("cmd. vararg. fully optional") {
    implicit val cmd = new Cmd(
      "program",
      indexedProps = VarArg("files"))

    // both specified
    Recognize.ast("a.txt", "b.txt").value should have (
      arg.set(0, "a.txt"),
      arg.set(1, "b.txt")
    )

    // only 0 specified
    Recognize.ast("a.txt").value should have (
      arg.set(0, "a.txt")
    )

    // nothing
    Recognize.ast(Array.empty[String]) shouldBe Symbol("ok")
  }

  test("cmd. vararg. with min occurrence") {
    implicit val cmd = new Cmd(
      "program",
      indexedProps = VarArg("files", minOcc = 1))

    // both specified
    Recognize.ast("a.txt", "b.txt").value should have (
      arg.set(0, "a.txt"),
      arg.set(1, "b.txt")
    )

    // only 0 specified
    Recognize.ast("a.txt").value should have (
      arg.set(0, "a.txt")
    )

    // nothing
    Recognize.ast(Array.empty[String]) shouldBe RecognizeErr.MissingRequiredProps(Seq("<..files> (still requires at least 1)"))
  }

  test("cmd. mixed arg + vararg") {
    implicit val cmd = new Cmd(
      "copy",
      indexedProps = Arg("from") :+ Arg("to") :+ VarArg("masks", minOcc = 1))

    // all 3 and more specified
    Recognize.ast("/from", "/to", "*.txt", "*.pages", "*.pdf").value should have (
      arg.set(0, "/from"),
      arg.set(1, "/to"),
      arg.set(2, "*.txt"),
      arg.set(3, "*.pages"),
      arg.set(4, "*.pdf")
    )

    // all 3 specified
    Recognize.ast("/from", "/to", "*.*").value should have (
      arg.set(0, "/from"),
      arg.set(1, "/to"),
      arg.set(2, "*.*")
    )

    // only 2 specified
    Recognize.ast("/from", "/to") shouldBe RecognizeErr.MissingRequiredProps(Seq("<..masks> (still requires at least 1)"))

    // only 1 specified
    Recognize.ast("/from") shouldBe RecognizeErr.MissingRequiredProps(Seq("<to>", "<..masks> (still requires at least 1)"))
  }

  test("cmd, cmd") {
    val copy = new Cmd(
      "copy",
      keyedProps = Flag('r') :+ Flag('f'),
      indexedProps = Arg("from") :+ Arg("to").!)

    implicit val cmd = {
      val open = new Cmd(
        "open",
        indexedProps = Arg("file").!)

      new Cmd(
        "fs",
        keyedProps = Flag('v'),
        indexedProps = open :+ copy)
    }

    // preceding flag
    Recognize.ast("-v", "copy", "-r", "/from/file.txt", "/to/file.txt").value should have (
      flag.set('v'),
      subcmd.suchThat { case (subCmd, subRes) =>
        subCmd.program shouldBe "copy"
        subRes should have (
          flag.set('r'),
          flag.unset('f'),
          arg.set(0, "/from/file.txt"),
          arg.set(1, "/to/file.txt")
        )
      }
    )

    // no preceding flag
    Recognize.ast("copy", "/from/file.txt", "/to/file.txt").value should have (
      flag.unset('v'),
      subcmd.suchThat { case (subCmd, subRes) =>
        subCmd.program shouldBe "copy"
        subRes should have (
          flag.unset('r'),
          flag.unset('f'),
          arg.set(0, "/from/file.txt"),
          arg.set(1, "/to/file.txt")
        )
      }
    )

    // unknown command
    Recognize.ast("move") shouldBe RecognizeErr.UnknownIndexedProp("move")

    // lack of arguments
    Recognize.ast("copy", "/from") shouldBe RecognizeErr.SubError(copy, RecognizeErr.MissingRequiredProps(Seq("<to>")))
  }

  test("flags. grouped. positive") {
    implicit val cmd = new Cmd(
      "program",
      keyedProps = Flag('a') :+ Flag('b') :+ Flag('c'))

    Recognize.ast("-abc").value should have (
      flag.set('a'),
      flag.set('b'),
      flag.set('c')
    )

    Recognize.ast("-a", "-bc").value should have (
      flag.set('a'),
      flag.set('b'),
      flag.set('c')
    )

    Recognize.ast("-ab", "-c").value should have (
      flag.set('a'),
      flag.set('b'),
      flag.set('c')
    )
  }


  test("flags. grouped. negative") {
    implicit val cmd = new Cmd(
      "program",
      keyedProps = Flag('a') :+ Flag('b') :+ Flag('c'))

    Recognize.ast("-abx") shouldBe RecognizeErr.UnknownProperty("x")

    Recognize.ast("-a", "-bx") shouldBe RecognizeErr.UnknownProperty("x")

    Recognize.ast("-ab", "-x") shouldBe RecognizeErr.UnknownProperty("x")
  }

  test("flags. repetitive. positive") {
    implicit val cmd = new Cmd(
      "program",
      keyedProps = Flag('v').∞)

    Recognize.ast("-v", "-v").value should have { flag.set('v', 2) }
    Recognize.ast("-v", "-v", "-v").value should have { flag.set('v', 3) }
    Recognize.ast("-v", "-v", "-v", "-v").value should have { flag.set('v', 4) }
  }

  test("flags. repetitive. negative") {
    implicit val cmd = new Cmd(
      "program",
      keyedProps = Flag('v'))

    Recognize.ast("-v").value should have { flag.set('v') }
    Recognize.ast("-v", "-v") shouldBe RecognizeErr.RepetitionOfNonRepetitive(Flag('v'))
  }

  test("opts. repetitive. positive") {
    implicit val cmd = new Cmd(
      "program",
      keyedProps = Opt('v').∞)

    Recognize.ast("-v", "1", "-v", "2").value should have { opt.set('v', List("1", "2")) }
    Recognize.ast("-v", "1", "-v", "2", "-v", "3").value should have { opt.set('v', List("1", "2", "3")) }
    Recognize.ast("-v", "1", "-v", "2", "-v", "3", "-v", "4").value should have { opt.set('v', List("1", "2", "3", "4")) }
  }

  test("opts. repetitive. negative") {
    implicit val cmd = new Cmd(
      "program",
      keyedProps = Opt('v'))

    Recognize.ast("-v", "1").value should have { opt.set('v', "1") }
    Recognize.ast("-v", "1", "-v", "2") shouldBe RecognizeErr.RepetitionOfNonRepetitive(Opt('v'))
  }

  test("custom result") {
    import RecognizeSpec._

    implicit val cmd = new Cmd(
      "fs",
      keyedProps = Flag('v'|"verbose"),
      indexedProps =
        new Cmd("open", indexedProps = Arg("file").!) :+
        new Cmd("cat", indexedProps = Arg("file").!) :+
        new Cmd("head", keyedProps = Opt('n'|"lines-num"), indexedProps = Arg("file").!) :+
        new Cmd("tail", keyedProps = Opt('n'|"lines-num"), indexedProps = Arg("file").!))

    val init = FsApp(false, null)

    // file exists
    Recognize(init, FsApp.builder)("open", tempFile.toString).value shouldBe FsApp(false, FsCmd.Open(tempFile))
    Recognize(init, FsApp.builder)("cat", tempFile.toString).value shouldBe FsApp(false, FsCmd.Cat(tempFile))
    Recognize(init, FsApp.builder)("-v", "head", tempFile.toString, "-n", "20").value shouldBe FsApp(true, FsCmd.Head(tempFile, 20))
    Recognize(init, FsApp.builder)("-v", "tail", tempFile.toString, "-n", "200").value shouldBe FsApp(true, FsCmd.Tail(tempFile, 200))
  }
}

object RecognizeSpec {

  trait FsCmd

  final object FsCmd {

    final case class Open(file: Path) extends FsCmd

    final case class Cat(file: Path) extends FsCmd

    final case class Head(file: Path, linesNum: Int) extends FsCmd

    final case class Tail(file: Path, linesNum: Int) extends FsCmd

    val builder = new Builder.NoCmd[FsCmd] {

      private def withFile[T](init: T, value: String)(fn: (T, Path) => T): Effect[T] = {
        val path = Paths.get(value)
        if (Files.exists(path)) Ok(fn(init, path)) else RecognizeErr.Custom(s"File not found: $value")
      }

      override def withFlag(
        init: FsCmd,
        flag: Flag): Effect[FsCmd] = RecognizeErr.UnexpectedFlag(flag)

      override def withOpt(
        init: FsCmd,
        opt: Opt,
        value: String): Effect[FsCmd] = if (opt.key =:= 'n') {
        for {
          n <- try Ok(value.toInt) catch {
            case err: NumberFormatException => RecognizeErr.FormatError(err.getMessage)
          }
          r <- init match {
            case cmd: Head => Ok(cmd.copy(linesNum = n))
            case cmd: Tail => Ok(cmd.copy(linesNum = n))
            case cmd       => RecognizeErr.IllegalState(cmd)
          }
        } yield r
      } else RecognizeErr.UnexpectedOpt(opt, value)

      override def withArg(
        init: FsCmd,
        arg: Arg,
        value: String): Effect[FsCmd] = {
        init match {
          case cmd@Open(null)    => withFile[Open](cmd, value) { case (c, f) => c.copy(file = f) }
          case Open(x)           => RecognizeErr.ArgAlreadySet(arg, value)
          case cmd@Cat(null)     => withFile[Cat](cmd, value) { case (c, f) => c.copy(file = f) }
          case Cat(x)            => RecognizeErr.ArgAlreadySet(arg, value)
          case cmd@Head(null, _) => withFile[Head](cmd, value) { case (c, f) => c.copy(file = f) }
          case Head(x, _)        => RecognizeErr.ArgAlreadySet(arg, value)
          case cmd@Tail(null, _) => withFile[Tail](cmd, value) { case (c, f) => c.copy(file = f) }
          case Tail(x, _)        => RecognizeErr.ArgAlreadySet(arg, value)
        }
      }
    }
  }

  case class FsApp(
    verbose: Boolean,
    cmd: FsCmd)

  final object FsApp {
    private implicit val fsCmdAdapter: SubCommandAdapter[FsApp] = new SubCommandAdapter[FsApp] {
      type Command = FsCmd
      override def init(cmd: Cmd): Effect[(FsCmd, Builder[FsCmd])] = cmd.program match {
        case "open" => Ok((FsCmd.Open(null), FsCmd.builder))
        case "cat"  => Ok((FsCmd.Cat(null), FsCmd.builder))
        case "head" => Ok((FsCmd.Head(null, -1), FsCmd.builder))
        case "tail" => Ok((FsCmd.Tail(null, -1), FsCmd.builder))
        case _      => RecognizeErr.UnknownCmd(cmd)
      }
    }

    val builder = new Builder[FsApp]()(fsCmdAdapter) {
      override def withFlag(init: FsApp, flag: Flag): Effect[FsApp] = if (flag.key =:= 'v') Ok(init.copy(verbose = true)) else RecognizeErr.UnexpectedFlag(flag)
      override def withOpt(init: FsApp, opt: Opt, value: String): Effect[FsApp] = RecognizeErr.UnexpectedOpt(opt, value)
      override def withArg(init: FsApp, arg: Arg, value: String): Effect[FsApp] = RecognizeErr.UnexpectedArg(arg, value)
      override def withSubCmd(init: FsApp, cmd: Cmd, value: sub.Command): Effect[FsApp] = Ok(init.copy(cmd = value.asInstanceOf[FsCmd]))
    }
  }
}
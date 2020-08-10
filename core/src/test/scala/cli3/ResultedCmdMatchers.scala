package cli3

import cli3. { Def => D, Res => R }
import org.scalactic.source.Position
import org.scalatest.exceptions.{StackDepthException, TestFailedException}
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher}

trait ResultedCmdMatchers {

  object flag {

    def set(k: Key)(implicit pos: Position): HavePropertyMatcher[R.Cmd, Boolean] = HavePropertyMatcher { cmd =>
      val actual = cmd.flags.get(k) getOrElse {
        throw new TestFailedException((_: StackDepthException) => Some(s"Flag $k is not defined for this command"), None, pos)
      }

      HavePropertyMatchResult(
        actual > 0,
        k.toString,
        true,
        actual > 0)
    }

    def set(k: Key, v: Int)(implicit pos: Position): HavePropertyMatcher[R.Cmd, Boolean] = HavePropertyMatcher { cmd =>
      val actual = cmd.flags.get(k) getOrElse {
        throw new TestFailedException((_: StackDepthException) => Some(s"Flag $k is not defined for this command"), None, pos)
      }

      HavePropertyMatchResult(
        actual == v,
        k.toString,
        true,
        actual == v)
    }

    def unset(k: Key)
      (implicit pos: Position): HavePropertyMatcher[R.Cmd, Boolean] = HavePropertyMatcher { cmd =>
      val actual = cmd.flags.get(k) getOrElse {
        throw new TestFailedException((_: StackDepthException) => Some(s"Flag $k is not defined for this command"), None, pos)
      }

      HavePropertyMatchResult(
        actual == 0,
        k.toString,
        false,
        actual == 0)
    }
  }

  object opt {

    def set(k: Key, expect: String)(implicit pos: Position): HavePropertyMatcher[R.Cmd, String] = HavePropertyMatcher { cmd =>
      val actual = cmd.opts.get(k) getOrElse {
        throw new TestFailedException((_: StackDepthException) => Some(s"Opt $k is not defined for this command"), None, pos)
      }

      HavePropertyMatchResult(
        actual contains expect,
        k.toString,
        expect,
        actual mkString ", ")
    }

    def set(k: Key, expect: List[String])(implicit pos: Position): HavePropertyMatcher[R.Cmd, List[String]] = HavePropertyMatcher { cmd =>
      val actual = cmd.opts.get(k) getOrElse {
        throw new TestFailedException((_: StackDepthException) => Some(s"Opt $k is not defined for this command"), None, pos)
      }

      val matches = actual.foldLeft(expect.toVector) {
        case (rest, e) =>
          var found = false
          rest flatMap {
            case r if r == e && !found => found = true; None
            case r => Some(r)
          }
      }.isEmpty

      println(
        s"""!!! $matches
           |!!! expect $expect
           |!!! actual $actual
           |""".stripMargin)
      HavePropertyMatchResult(
        matches,
        k.toString,
        expect,
        actual)
    }

    def empty(k: Key)(implicit pos: Position): HavePropertyMatcher[R.Cmd, Boolean] = HavePropertyMatcher { cmd =>
      val actual = cmd.opts.get(k) getOrElse {
        throw new TestFailedException((_: StackDepthException) => Some(s"Opt $k is not defined for this command"), None, pos)
      }

      HavePropertyMatchResult(
        actual.isEmpty,
        k.toString,
        true,
        actual.isEmpty)
    }
  }

  object arg {

    def set(i: Int, expect: String)(implicit pos: Position): HavePropertyMatcher[R.Cmd, String] = HavePropertyMatcher { cmd =>
      val actual = cmd.args.get(i) match {
        case Right(x)  => x
        case Left(err) =>
          throw new TestFailedException((_: StackDepthException) => Some(s"Arg $i is not defined for this command"), None, pos)
      }

      HavePropertyMatchResult(
        actual contains expect,
        i.toString,
        expect,
        actual getOrElse "")
    }

    def unset(i: Int)(implicit pos: Position): HavePropertyMatcher[R.Cmd, Boolean] = HavePropertyMatcher { cmd =>
      val actual = cmd.args.get(i) match {
        case Right(x)  => x
        case Left(err) =>
          throw new TestFailedException((_: StackDepthException) => Some(s"Arg $i is not defined for this command"), None, pos)
      }

      HavePropertyMatchResult(
        actual.isEmpty,
        i.toString,
        true,
        actual.isEmpty)
    }
  }

  object subcmd {

    def suchThat(fn: (D.Cmd, R.Cmd) => Unit)(implicit pos: Position): HavePropertyMatcher[R.Cmd, Boolean] = HavePropertyMatcher { cmd =>
      val (actualCmd, actualRes) = cmd.cmd getOrElse {
          throw new TestFailedException((_: StackDepthException) => Some(s"Sub-Command is not defined for this command"), None, pos)
      }

      fn(actualCmd, actualRes)

      HavePropertyMatchResult(
        matches = true,
        "sub-command",
        true,
        true)
    }

  }
}

object ResultedCmdMatchers extends ResultedCmdMatchers
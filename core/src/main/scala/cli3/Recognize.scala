package cli3

import cli3. { Def => D, Res => R }

object Recognize {

  private implicit class Ops[T](private val x: T) extends AnyVal {

    def selfMap(fn: T => T) = fn(x)
  }

  private[cli3] def unquote(x: String): Either[RecognizeError, String] = {
    if (x.head.isQ) {
      if (x.length == 1 || x.last != x.head) Left(RecognizeError.UnquotedString(x))
      else Right(x.drop(1).dropRight(1))
    } else Right(x)
  }

  private[cli3] def key(x: String): Either[RecognizeError, String] = for {
    h <- x.headOption match {
           case Some(c) if !c.isLetterOrDigit => Left(RecognizeError.IllegalFirstChar(c))
           case Some(c)                       => Right(c)
           case None                          => Left(RecognizeError.EmptyString)
         }
    t  = x.tail.takeWhile { x => x.isLetterOrDigit || x == '_' || x == '.' || x == '-' }
    _ <- t.lastOption match {
      case Some(c) if !c.isLetterOrDigit => Left(RecognizeError.IllegalLastChar(c))
      case _                             => Right(())
    }
  } yield h +: t

  private[cli3] def keyed(x: String, cmd: D.Cmd): Either[RecognizeError, (D.Keyed, Option[String])] = for {
    k <- key(x)
    p <- cmd.resolveKeyed(k) toRight RecognizeError.UnknownProperty(k)
    r  = x.drop(k.length).trim selfMap { x => if (x.headOption.contains('=')) x.drop(1) else x }
    _ <- if (p.isInstanceOf[D.Flag] && x.length > k.length) Left(RecognizeError.FlagWithValue(p.asInstanceOf[D.Flag], r)) else Right(())
    v  <- if (r.nonEmpty) unquote(r).map(Some.apply) else Right(None)
  } yield (p, v)

  def apply[T](init: T, bld: Builder[T])(x: String, xs: String*)(implicit defn: D.Cmd): Either[RecognizeError, T] = Recognize(init, bld, (x +: xs).toArray)

  def apply[T](init: T, bld: Builder[T], xs: Array[String])(implicit defn: D.Cmd): Either[RecognizeError, T] = cmd(xs, defn, init, bld)

  def ast(x: String, xs: String*)(implicit defn: D.Cmd): Either[RecognizeError, Res.Cmd] = ast((x +: xs).toArray)(defn)

  def ast(xs: Array[String])(implicit defn: D.Cmd): Either[RecognizeError, Res.Cmd] = {
    val init = Res.Cmd(defn)
    val bld  = new Res.Builder()

    cmd(xs, defn, init, bld)
  }

  private[cli3] def cmd[T](
    xs: Array[String],
    defn: D.Cmd,
    init: T,
    bld: Builder[T]): Either[RecognizeError, T] = {

    parseCmd(
      xs,
      defn,
      init,
      bld)
  }

  // recursive parsing of command line arguments
  private def parseCmd[T](
    xs: Array[String],
    defn: D.Cmd,
    res: T,
    bld: Builder[T]): Either[RecognizeError, T] = {

    def handleKeyed(str: String) = {
      Recognize.keyed(str, defn) match {
        // --flag
        case Right((p: D.Flag, None))    => for {
          u <- bld.withFlag(res, p)
          d <- defn.occurred(p)
          r <- parseCmd(xs.tail, d, u, bld)
        } yield r
        // --opt=val
        case Right((p: D.Opt, Some(v)))  => for {
          v <- unquote(v)
          u <- bld.withOpt(res, p, v)
          d <- defn.occurred(p)
          r <- parseCmd(xs.tail, d, u, bld)
        } yield r
        // --opt
        // assuming the value comes as next element of `xs` array
        case Right((p: D.Opt, None)) if xs.tail.nonEmpty =>
          for {
            v <- unquote(xs.tail.head)
            u <- bld.withOpt(res, p, v)
            d <- defn.occurred(p)
            r <- parseCmd(xs.tail.tail, d, u, bld)
          } yield r
        // option without value
        case Right((p: D.Opt, None)) => Left(RecognizeError.OptionWithoutValue(p))
        // error
        case Left(err) => Left(err)
      }
    }

    def handleIndexed(str: String) = {
      defn.resolveIndexed(str) match {
        // args
        case Some(a: D.Arg) => for {
          u <- bld.withArg(res, a, str)
          d <- defn.occurred(a)
          r <- parseCmd(xs.tail, d, u, bld)
        } yield r

        // cmds
        case Some(c: D.Cmd) => for {
          init       <- bld.init(c)
          (cc, ccBld) = init
          r          <- parseCmd(xs.tail, c, cc, ccBld) match {
                         case Right(resCmd) => bld.withSubCmd(res, c, resCmd)
                         case Left(err)     => Left(RecognizeError.SubError(c, err))
                       }
        } yield r

        // the rest
        case None => Left(RecognizeError.UnknownIndexedProp(str))
      }
    }

    xs.map(_.trim).filterNot(_.isEmpty).headOption match {
      case Some(x) if x.startsWith("--")                 => handleKeyed(x.substring(2))
      case Some(x) if x.startsWith("-") && x.length == 2 => handleKeyed(x.substring(1))
      case Some(x) if x.startsWith("-")                  => Left(RecognizeError.IllegalSyntax(x))
      case Some(x)                                       => for { x <- unquote(x); r <- handleIndexed(x) } yield r
      case None =>
        val props = defn.requiredProps flatMap {
          case k: D.Keyed      => Some(k.key.toString)
          case a: D.Arg.Arg1   => Some(s"<$a>")
          case a: D.Arg.VarArg => if (a.minOcc > 0) Some(s"<$a> (still requires at least ${a.minOcc})") else None
          case _               => None
        }

        if (props.isEmpty) Right(res) else {
          Left(RecognizeError.MissingRequiredProps(props))
        }
    }
  }
}

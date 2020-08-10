package cli3

import scala.util.control.NoStackTrace

private[cli3] object Tokenize {

  private case class TokenizeException(error: TokenizeError) extends Exception with NoStackTrace

  def apply(x: String): Either[CliError, Seq[String]] = Tokenize(x.toCharArray)

  def apply(x: Array[Char]): Either[CliError, Seq[String]] = {
    val len = x.length
    var pos = 0

    def isEOL: Boolean = pos >= len

    def isQ: Boolean = x(pos).isQ

    def readToken: String = {
      val start = pos
      while(!isEOL && !Character.isWhitespace(x(pos))) {
        if (isQ) readString else pos += 1
      }
      new String(x, start, pos - start)
    }

    def skipWS(): Unit = {
      while(!isEOL && Character.isWhitespace(x(pos))) pos += 1
    }

    // TODO: add support for \"
    def readString: String = {
      val start = pos
      val q = x(pos) // remember first char. one of quote [`"']
      pos += 1 // head Quote
      while(!isEOL && q != x(pos)) pos += 1
      if (!isEOL) pos += 1 else throw TokenizeException(TokenizeError.UnexpectedEOL)// trailing Quote
      new String(x, start, pos - start)
    }

    var tokens = Vector.empty[String]
    while(!isEOL) {
      skipWS()
      if (!isEOL) try {
        tokens = tokens :+ (if (isQ) readString else readToken)
      } catch {
        case TokenizeException(err) => return Left(err)
      }
    }

    Right(tokens)
  }
}

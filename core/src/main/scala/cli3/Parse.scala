package cli3

object Parse {

  def ast(x: String)(implicit cmd: Def.Cmd): Effect[Res.Cmd] = {
    val init = Res.Cmd(cmd)
    val bld  = new Res.Builder()

    Parse(init, bld, x)
  }

  def apply[T](init: T, bld: Builder[T], x: String)(implicit cmd: Def.Cmd): Effect[T] = for {
    tokens <- Tokenize(x)
    cmd    <- Recognize(init, bld, tokens.toArray)
  } yield cmd
}

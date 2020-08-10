package cli3

import org.scalactic.source.Position
import org.scalatest.exceptions.{StackDepthException, TestFailedException}


object EffectValues {

  implicit class OkValue[T](private val x: Effect[T]) extends AnyVal {

    def value(implicit pos: Position): T = x getOrElse {
      throw new TestFailedException((_: StackDepthException) => Some(s"Expected Ok but got ${x}"), None, pos)
    }
  }
}

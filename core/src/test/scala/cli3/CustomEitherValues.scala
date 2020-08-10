package cli3

import org.scalactic.source.Position
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

trait CustomEitherValues {
}

object CustomEitherValues extends CustomEitherValues {

  implicit class ValueOfEither[L, R](private val x: Either[L, R]) extends AnyVal {

    def value(implicit pos: Position): R = x getOrElse {
      throw new TestFailedException((_: StackDepthException) => Some(s"Expected Right but got ${x}"), None, pos)
    }
  }
}

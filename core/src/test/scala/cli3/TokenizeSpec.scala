package cli3

import org.scalatest.matchers.should.Matchers._
import org.scalatest.EitherValues._
import org.scalatest.wordspec._
import CustomEitherValues._


class TokenizeSpec extends AnyWordSpec {

  "Tokenize" should {

    "handle" when {

      "empty line" in {
        Tokenize("").value should have size(0)
      }

      "line of spaces" in {
        Tokenize(" \n\t \n\n ").value should have size(0)
      }

      "unquoted arguments" in {
        Tokenize("abc def").value should contain only ( "abc", "def" )
      }

      "quoted arguments" in {
        Tokenize(""" "abc" "def" """).value should contain only ( "\"abc\"", "\"def\"" )
        Tokenize(""" 'abc' 'def' """).value should contain only ( "'abc'", "'def'" )
        Tokenize(""" `abc` `def` """).value should contain only ( "`abc`", "`def`" )

        // other quotes doesn't terminate
        Tokenize(""" "abc''" "`def`" """).value should contain only ( "\"abc''\"", "\"`def`\"" )
        Tokenize(""" '""abc' '``def' """).value should contain only ( "'\"\"abc'", "'``def'" )
        Tokenize(""" `"''abc"` `'"def'"` """).value should contain only ( "`\"''abc\"`", "`'\"def'\"`" )

        // unterminated strings
        Tokenize(""" "abc""").left.value shouldBe TokenizeError.UnexpectedEOL
        Tokenize(""" 'abc""").left.value shouldBe TokenizeError.UnexpectedEOL
        Tokenize(""" `abc""").left.value shouldBe TokenizeError.UnexpectedEOL
      }

      "short flags" in {
        val args = Tokenize(" -c -c -v").value
        args should have size (3)
        args should contain only ( "-c", "-v" )
      }

      "long flags" in {
        val args = Tokenize(" --color --verbose --verbose").value
        args should have size (3)
        args should contain only ( "--color", "--verbose" )
      }

      "short opts. unquoted" in {
        val args = Tokenize(" -x abc -y def").value
        args should have size (4)
        args shouldBe Seq("-x", "abc", "-y", "def")
      }

      "short opts. quoted" in {
        val args = Tokenize(" -x 'abc' -y `def`").value
        args shouldBe Seq("-x", "'abc'", "-y", "`def`")
      }

      "short opts. quoted (=)" in {
        val args = Tokenize(" -x='abc' -y=`def`").value
        args shouldBe Seq("-x='abc'", "-y=`def`")
      }

      "long opts. unquoted" in {
        val args = Tokenize(" --x-xx abc --y-yy def").value
        args should have size (4)
        args shouldBe Seq("--x-xx", "abc", "--y-yy", "def")
      }

      "long opts. quoted" in {
        val args = Tokenize(" --x-xx 'abc' --y-yy `def`").value
        args shouldBe Seq("--x-xx", "'abc'", "--y-yy", "`def`")
      }

      "long opts. quoted (=)" in {
        val args = Tokenize(" --x-xx='abc' --y-yy=`def`").value
        args shouldBe Seq("--x-xx='abc'", "--y-yy=`def`")
      }
    }
  }
}

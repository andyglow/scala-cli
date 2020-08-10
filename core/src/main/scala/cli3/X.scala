package cli3

import cli3.Def._

object X {

  def main(args: Array[String]): Unit = {
    val sparkCli = for {
      open  <- Cmd("open", header = Some("Opens SparkShell with current dataframe mounted"))
      run   <- Cmd("run", header = Some("Runs script in a SparkShell"), indexedProps = Arg("script"))
      spark <- Cmd("spark", header = Some("Spark Interaction"), indexedProps = open :+ run)
    } yield spark

    for {
      sparkCli  <- sparkCli
      hpCli <- Cmd("help")
      lsCli <- Cmd("ls", indexedProps = Arg("folder"), keyedProps = Flag('s' | "show-size"))
      cdCli <- Cmd("cd", indexedProps = Arg("folder"))
        cli <- Cmd(
                 "x",
                 title = Some("App"),
                 version = Some("1.0"),
                 keyedProps = Flag('v') :+ Opt("ccc").text("Kherasima Ibukidze").! :+ Opt('x' | "xxx-xxx"),
                 indexedProps =
                   hpCli :+
                   lsCli :+
                   cdCli :+
                   sparkCli)
    } {
      val print = PrintUsage()
      print(cli, System.out)
    }
  }
}

package  example

import cats.effect._
import io.chrisdavenport.hpack.HeaderField

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    IO(println(HeaderField("foo", "bar"))).as(ExitCode.Success)
  }

}
import zhttp.http._
import zhttp.service.Server
import zio._

object HelloWorld extends App {

  // Create HTTP route
  val app: HttpApp[Any, Nothing] = HttpApp.collect {
    case Method.GET -> "text" /: _ => Response.text("Hello World!")
    case Method.GET -> "json" /: _ => Response.jsonString("""{"greetings": "Hello World!"}""")
  }

  // Run it like any simple app
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8090, app.silent).exitCode
}

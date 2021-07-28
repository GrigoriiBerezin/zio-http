package zhttp.service.server

import io.netty.handler.codec.http.{HttpServerKeepAliveHandler => JHttpServerKeepAliveHandler}
import io.netty.handler.flow.{FlowControlHandler => JFlowControlHandler}
import zhttp.core._
import zhttp.experiment.Channel
import zhttp.service.Server.Settings
import zhttp.service._

/**
 * Initializes the netty channel with default handlers
 */
@JSharable
final case class ServerChannelInitializer[R](zExec: UnsafeChannelExecutor[R], settings: Settings[R, Throwable])
    extends JChannelInitializer[JChannel] {
  override def initChannel(channel: JChannel): Unit = {
    val sslctx = if (settings.sslOption == null) null else settings.sslOption.sslContext
    if (sslctx != null) {
      channel
        .pipeline()
        .addFirst(
          SSL_HANDLER,
          new OptionalSSLHandler(sslctx, settings.sslOption.httpBehaviour),
        )
      ()
    }
    channel
      .pipeline()
      .addLast(new JHttpServerCodec)
      .addLast(new JFlowControlHandler())
      .addLast(new JHttpServerKeepAliveHandler)
      .addLast(HTTP_REQUEST_HANDLER, Channel.compile(zExec, settings.channel))
    ()
  }

}

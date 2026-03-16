package gsoc
package contributors

import cats.effect.*
import cats.effect.std.Random
import fs2.concurrent.SignallingRef
import calico.html.io.{*, given}
import calico.syntax.*
import scala.scalajs.js
import scala.concurrent.duration.*
import org.scalajs.dom.HTMLCanvasElement

val richochetclementine1315: Contributor = Contributor("richochetclementine1315"):

  SignallingRef[IO].of((50.0, 50.0, 2.0, 3.0, "red")).toResource.flatMap { state =>
    Random.scalaUtilRandom[IO].toResource.flatMap { _ =>

      def update(w: Double, h: Double): IO[Unit] =
        state.modify { case (x, y, dx, dy, color) =>
          var nx = x + dx
          var ny = y + dy
          var ndx = dx
          var ndy = dy
          var ncolor = color

          if nx <= 0 || nx >= w then
            ndx = -dx
            ncolor = if color == "red" then "blue" else "red"
          if ny <= 0 || ny >= h then
            ndy = -dy
            ncolor = if color == "red" then "green" else "yellow"

          ((nx, ny, ndx, ndy, ncolor), ())
        }

      val canvasId = "richochet-bounce-canvas"

      div(
        p("Hello, I'm @richochetclementine1315 on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."),
        canvasTag(
          idAttr := canvasId,
          widthAttr := 400,
          heightAttr := 300
        ),
        (for
          _ <- IO.sleep(50.millis)
          rawCanvas = org.scalajs.dom.document
                        .getElementById(canvasId)
                        .asInstanceOf[HTMLCanvasElement]
          ctx = rawCanvas.getContext("2d").asInstanceOf[js.Dynamic]
          _ <- (for
                  _ <- update(400, 300)
                  (x, y, _, _, color) <- state.get
                  _ <- IO {
                    ctx.clearRect(0, 0, 400, 300)
                    ctx.fillStyle = color
                    ctx.beginPath()
                    ctx.arc(x, y, 20, 0, 2 * Math.PI)
                    ctx.fill()
                  }
                  _ <- IO.sleep(16.millis)
                yield ()).foreverM
        yield ()).start.void.toResource
      )
    }
  }


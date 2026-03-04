package gsoc
package contributors

import cats.effect.*
import cats.syntax.all.*

import fs2.*
import fs2.concurrent.*
import fs2.dom.HtmlElement

import calico.html.io.{*, given}
import calico.syntax.*

import scala.concurrent.duration.DurationInt
import scala.util.matching.Regex

val signals = (
  SignallingRef[IO].of(false),
  SignallingRef[IO].of(""),
  SignallingRef[IO].of(0.0),
  SignallingRef[IO].of(0.0)
).tupled.toResource

val updateMs = 8
val decelRate = 0.06
val decelChValue = decelRate * updateMs
val speedMod = 0.1
val speedScale = 25

def PosUpdater(speed: SignallingRef[IO, Double], pos: SignallingRef[IO, Double]) =
  Stream
    .fixedRate[IO](updateMs.millis)
    .evalMap(_ =>
      speed
        .updateAndGet(s => List(s - decelChValue, 0).max)
        .flatMap(s => pos.update(p => (p + s * speedMod) % 200)))
    .compile
    .drain
    .background

def countOs(text: String): Int =
  "VRO+M".r.findFirstIn(text.toUpperCase) match
    case Some(t) => t.length - 3
    case None => 0

val blagogunev = Contributor("BlagoGunev"):
  for
    (revealed, vroom, speed, pos) <- signals
    _ <- PosUpdater(speed, pos)
    node <- div(
      p(
        "I am ",
        revealed.map(r => if r then "firebrick" else "inherit").changes.map { color =>
          span(
            styleAttr := s"color: $color; font-weight: bold",
            "@BlagoGunev"
          )
        },
        " on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."
      ),
      revealed.map(r =>
        if r then
          div(
            pos
              .changes
              .map(p =>
                div(
                  div(
                    span(
                      "🚗",
                      styleAttr := s"display: inline-block; font-size: 200%; transform: " + {
                        if p < 100 then "scaleX(-1)" else "inherit"
                      }
                    ),
                    styleAttr := s"position: relative; " + {
                      if p < 100 then s"left:${p}%" else s"left:${200 - p}%"
                    }
                  )
                )),
            span(
              label("Type VROOOOOM to see it go fast:"),
              input.withSelf { self =>
                (
                  placeholder := "VROOOOOM",
                  onInput --> (_.foreach(_ => self.value.get.flatMap(vroom.set)))
                )
              },
              button(
                "Go!",
                onClick --> (_.foreach(_ =>
                  vroom.get.flatMap(vro => speed.update(v => v + speedScale * countOs(vro)))))
              )
            )
          )
        else div("")),
      button(
        onClick --> (_.foreach(_ => revealed.update(!_))),
        revealed.map(r => if r then "Put my car in the garage" else "Click to see my car!")
      )
    )
  yield node

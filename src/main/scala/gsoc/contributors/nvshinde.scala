package gsoc
package contributors

import cats.effect.*
import cats.effect.std.Random
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.syntax.*

import org.scalajs.dom
import scala.concurrent.duration.DurationInt

val nvshinde: Contributor = Contributor("nvshinde"):
  import CatShooter.*
  for
    rng <- Random.scalaUtilRandom[IO].toResource
    mousePos <- randCol(rng).toResource
    ref <- SignallingRef[IO].of(init(mousePos)).toResource
    node <- div(
      tabIndex := 0,
      styleAttr := "outline:none;",
      p(
        "I am",
        span(
          styleAttr := "color:mediumseablue;font-weight:bold",
          "@nvshinde"
        ),
        " on Github. I agree to follow the Typelevel CoC and GSoC AI policy."
      ),
      p(
        styleAttr := "font-size:13px;color:#888;margin:4px 0;",
        "Play the Mice invaders game below. Use arrow keys to move left and right and space to shoot!"
      ),
      onKeyDown --> (_.foreach { e =>
        e.key match
          case "ArrowLeft" => e.preventDefault *> ref.update(moveLeft)
          case "ArrowRight" => e.preventDefault *> ref.update(moveRight)
          case " " => e.preventDefault *> shootLaser(ref, rng)
          case _ => IO.unit
      }),
      ref.map(renderSpace),
      ref.map { s =>
        p(
          styleAttr := "margin-top:8px;font-size:14px;",
          s"Score: ${s.score}"
        )
      }
    )
  yield node

private object CatShooter {
  val Size = 20
  val CellPx = 20

  final case class State(
      cat: Int,
      mouse: Int,
      laserRow: Option[Int],
      score: Int
  )

  def randCol(rng: Random[IO]): IO[Int] =
    rng.betweenInt(0, Size)

  def init(mouse: Int): State = State(
    cat = Size / 2,
    mouse = mouse,
    laserRow = None,
    score = 0
  )

  def moveLeft(s: State): State =
    if s.cat > 0 then s.copy(cat = s.cat - 1) else s

  def moveRight(s: State): State =
    if s.cat < Size - 1 then s.copy(cat = s.cat + 1) else s

  def shootLaser(
      ref: SignallingRef[IO, State],
      rng: Random[IO]
  ): IO[Unit] =
    fs2
      .Stream
      .range(1, Size)
      .evalMap { row => ref.update(_.copy(laserRow = Some(row))) }
      .metered(20.millis)
      .compile
      .drain >>
      ref
        .modify { s =>
          if s.cat == s.mouse then
            val updatedScore = s.copy(score = s.score + 1, laserRow = None)
            (updatedScore, true)
          else (s.copy(laserRow = None), false)
        }
        .flatMap {
          case true =>
            randCol(rng).flatMap(newCol => ref.update(_.copy(mouse = newCol)))
          case false => IO.unit
        }

  def renderSpace(s: State): Resource[IO, HtmlElement[IO]] =
    div(
      styleAttr := s"display:grid;grid-template-columns:repeat($Size, ${CellPx}px); background:#111;width:${Size * CellPx}px;height:${Size * CellPx}px"
    ).flatMap { gridEl =>
      Resource
        .eval(IO {
          val raw = gridEl.asInstanceOf[dom.HTMLElement]

          raw.innerHTML = ""
          for
            y <- 0 until Size
            x <- 0 until Size
          do
            val cell = dom.document.createElement("div")
            cell.setAttribute(
              "style",
              s"width:${CellPx}px;height:${CellPx}px;display:flex;align-items:center;justify-content:center;color:white;font-weight:bold;font-size:20px"
            )

            val content =
              if y == 0 && x == s.cat then "C"
              else if y == Size - 1 && x == s.mouse then "M"
              else if s.laserRow.contains(y) && x == s.cat then "*"
              else ""

            cell.textContent = content
            raw.appendChild(cell)
        })
        .map(_ => gridEl)
    }
}

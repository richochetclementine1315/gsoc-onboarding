package gsoc
package contributors

import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.frp.given
import calico.syntax.*
import scala.concurrent.duration.DurationInt
import scala.util.Random

// ── State lives at file scope (required for Scala 3 + Scala.js) ──────────────
private case class LifeState(
    cells: Set[(Int, Int)],
    ages: Map[(Int, Int), Int],
    gen: Int,
    running: Boolean,
    speed: Int // milliseconds per tick
)

// ─────────────────────────────────────────────────────────────────────────────

// ── What is Conway's Game of Life? ───────────────────────────────────────────
// A zero-player simulation on a grid of cells. Each cell is alive or dead.
// Every generation all cells update at once by 4 rules:
//   1. Live + <2 neighbours  → dies  (underpopulation)
//   2. Live + 2-3 neighbours → lives (survival)
//   3. Live + >3 neighbours  → dies  (overpopulation)
//   4. Dead + exactly 3 neighbours → born (reproduction)
// Simple rules, endlessly complex emergent behaviour.
// Cell colour = age: mint green = newborn, dark green = ancient.
// Controls: Play/Pause, Step (one gen at a time), Clear, speed buttons,
//           preset patterns, and click any cell to draw while running.
// ─────────────────────────────────────────────────────────────────────────────

val arnvptl: Contributor = Contributor("arnvptl"):

  val cols = 24
  val rows = 18

  // ── Conway rules ───────────────────────────────────────────────────────────
  def nb(x: Int, y: Int): List[(Int, Int)] =
    (for dx <- -1 to 1; dy <- -1 to 1 if dx != 0 || dy != 0
    yield (x + dx, y + dy)).filter {
      case (nx, ny) => nx >= 0 && nx < cols && ny >= 0 && ny < rows
    }.toList

  def tick(s: LifeState): LifeState =
    val alive = s.cells
    val candidates = alive.flatMap(nb) ++ alive
    val next = candidates.filter {
      case (x, y) =>
        val n = nb(x, y).count(alive.contains)
        if alive.contains((x, y)) then n == 2 || n == 3 else n == 3
    }
    LifeState(
      cells = next,
      ages = next.map(p => p -> (s.ages.getOrElse(p, 0) + 1)).toMap,
      gen = s.gen + 1,
      running = s.running,
      speed = s.speed
    )

  // ── Helpers ────────────────────────────────────────────────────────────────
  def cellColor(age: Int): String = age match
    case 1 => "#6ee7b7"
    case 2 | 3 => "#34d399"
    case n if n < 10 => "#10b981"
    case _ => "#047857"

  def offset(ox: Int, oy: Int, pts: Set[(Int, Int)]): Set[(Int, Int)] =
    pts.map { case (x, y) => (x + ox, y + oy) }.filter {
      case (x, y) => x >= 0 && x < cols && y >= 0 && y < rows
    }

  def makeState(pts: Set[(Int, Int)], speed: Int = 150): LifeState =
    LifeState(pts, pts.map(_ -> 1).toMap, 0, false, speed)

  def randomState(speed: Int): LifeState =
    val pts = (for
      x <- 0 until cols; y <- 0 until rows
      if Random.nextDouble() < 0.3
    yield (x, y)).toSet
    makeState(pts, speed)

  // ── Patterns ───────────────────────────────────────────────────────────────
  val glider = Set((1, 0), (2, 1), (0, 2), (1, 2), (2, 2))
  val rpent = Set((1, 0), (2, 0), (0, 1), (1, 1), (1, 2))
  val blinker = Set((0, 0), (1, 0), (2, 0))
  val toad = Set((1, 0), (2, 0), (3, 0), (0, 1), (1, 1), (2, 1))

  val patterns: List[(String, Set[(Int, Int)])] = List(
    "Glider" -> offset(1, 1, glider),
    "R-pent" -> offset(11, 8, rpent),
    "Blinker" -> offset(11, 8, blinker),
    "Toad" -> offset(9, 7, toad),
    "Random" -> Set.empty // handled specially
  )

  // ── Grid render (follows antoniojimeneznieto pattern) ──────────────────────
  // Returns Resource[IO, HtmlElement[IO]] so calico.frp.given can render
  // Signal[IO, Resource[IO, HtmlElement[IO]]] as a reactive child node.
  def renderGrid(
      s: LifeState,
      ref: SignallingRef[IO, LifeState]
  ): Resource[IO, HtmlElement[IO]] =
    div(
      styleAttr := s"display:grid;grid-template-columns:repeat($cols,16px);gap:1px;background:#1e293b;padding:3px;border-radius:6px;border:1px solid #334155;cursor:crosshair;user-select:none;",
      (for y <- 0 until rows; x <- 0 until cols yield (x, y)).toList.map {
        case (x, y) =>
          val isAlive = s.cells.contains((x, y))
          val bg = if isAlive then cellColor(s.ages.getOrElse((x, y), 1)) else "#0f172a"
          div(
            styleAttr := s"width:16px;height:16px;background:$bg;border-radius:2px;",
            onClick --> (_.foreach { _ =>
              ref.update { gs =>
                val p = (x, y)
                if gs.cells.contains(p) then gs.copy(cells = gs.cells - p, ages = gs.ages - p)
                else gs.copy(cells = gs.cells + p, ages = gs.ages + (p -> 1))
              }
            })
          )
      }
    )

  // ── Wire everything up ─────────────────────────────────────────────────────
  val seed = offset(1, 1, glider) ++ offset(11, 6, rpent)

  SignallingRef[IO].of(makeState(seed)).toResource.flatMap { state =>

    val loop: IO[Unit] =
      state
        .get
        .flatMap { s =>
          IO.sleep(s.speed.millis) >>
            state.update(s => if s.running then tick(s) else s)
        }
        .foreverM

    loop.background >> div(
      styleAttr := "font-family:'Segoe UI',system-ui,sans-serif;background:#0f172a;color:#e2e8f0;padding:20px;border-radius:12px;max-width:480px;border:1px solid #1e293b;box-shadow:0 8px 24px rgba(0,0,0,0.6);",

      // Identity + CoC
      p(
        styleAttr := "margin:0 0 2px 0;font-size:0.85em;color:#94a3b8;",
        "Hello, I'm ",
        span(styleAttr := "color:#34d399;font-weight:700;", "@arnvptl"),
        " on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."
      ),
      h3(
        styleAttr := "margin:10px 0 6px 0;font-size:1em;color:#34d399;letter-spacing:1px;",
        "Conway's Game of Life"
      ),

      // Short description
      p(
        styleAttr := "margin:0 0 8px 0;font-size:0.78em;color:#64748b;line-height:1.5;",
        "A zero-player simulation — set a pattern, hit Play, watch complexity emerge from 4 simple rules: ",
        span(
          styleAttr := "color:#94a3b8;",
          "< 2 neighbours → dies, 2–3 → survives, > 3 → dies, dead + exactly 3 → born."),
        " Darker green = older cell. Click any cell to draw while running."
      ),

      // Stats row
      div(
        styleAttr := "display:flex;gap:16px;margin-bottom:6px;font-size:0.78em;color:#64748b;",
        state.map(s => span(s"gen ${s.gen}")),
        state.map(s => span(s"pop ${s.cells.size}")),
        state.map(s =>
          span(
            styleAttr := s"color:${if s.running then "#34d399" else "#f87171"};",
            if s.running then "● live" else "■ paused"
          ))
      ),

      // Grid — Signal[IO, Resource[IO, HtmlElement[IO]]]
      state.map(s => renderGrid(s, state)),

      // Play / Pause  ·  Step  ·  Clear
      div(
        styleAttr := "margin-top:10px;display:flex;gap:6px;flex-wrap:wrap;",
        button(
          styleAttr := "padding:5px 14px;border-radius:6px;border:none;cursor:pointer;font-size:0.83em;background:#16a34a;color:#fff;font-weight:600;",
          onClick --> (_.foreach(_ => state.update(s => s.copy(running = !s.running)))),
          state.map(s => if s.running then "⏸ Pause" else "▶ Play")
        ),
        button(
          styleAttr := "padding:5px 14px;border-radius:6px;border:1px solid #334155;cursor:pointer;font-size:0.83em;background:#1e293b;color:#e2e8f0;",
          onClick --> (_.foreach(_ => state.update(s => tick(s).copy(running = false)))),
          "⏭ Step"
        ),
        button(
          styleAttr := "padding:5px 14px;border-radius:6px;border:1px solid #334155;cursor:pointer;font-size:0.83em;background:#1e293b;color:#e2e8f0;",
          onClick --> (_.foreach(_ => state.update(s => makeState(Set.empty, s.speed)))),
          "✕ Clear"
        )
      ),

      // Speed selector
      div(
        styleAttr := "margin-top:7px;display:flex;gap:5px;align-items:center;font-size:0.78em;color:#64748b;",
        span("Speed:"),
        List(("Slow", 300), ("Med", 150), ("Fast", 50)).map { (lbl, ms) =>
          button(
            styleAttr <-- state.map(s =>
              val on = s.speed == ms
              s"padding:3px 10px;border-radius:5px;cursor:pointer;font-size:0.8em;color:#e2e8f0;" +
                s"background:${if on then "#1d4ed8" else "#1e293b"};" +
                s"border:1px solid ${if on then "#3b82f6" else "#334155"};"),
            onClick --> (_.foreach(_ => state.update(_.copy(speed = ms)))),
            lbl
          )
        }
      ),

      // Pattern presets
      div(
        styleAttr := "margin-top:7px;display:flex;gap:5px;align-items:center;flex-wrap:wrap;font-size:0.78em;color:#64748b;",
        span("Load:"),
        patterns.map { (name, pts) =>
          button(
            styleAttr := "padding:3px 10px;border-radius:5px;border:1px solid #334155;cursor:pointer;font-size:0.8em;background:#1e293b;color:#e2e8f0;",
            onClick --> (_.foreach { _ =>
              state.update { s =>
                val chosen = if name == "Random" then randomState(s.speed).cells else pts
                makeState(chosen, s.speed)
              }
            }),
            name
          )
        }
      ),

      // Age legend + hint
      div(
        styleAttr := "margin-top:9px;display:flex;gap:8px;font-size:0.7em;color:#475569;align-items:center;flex-wrap:wrap;",
        span("Age:"),
        List(("New", "#6ee7b7"), ("Mid", "#34d399"), ("Old", "#10b981"), ("Ancient", "#047857"))
          .map { (lbl, col) =>
            span(
              styleAttr := "display:flex;align-items:center;gap:3px;",
              span(
                styleAttr := s"display:inline-block;width:9px;height:9px;background:$col;border-radius:2px;",
                ""),
              lbl
            )
          },
        span(styleAttr := "margin-left:4px;", "· click grid to draw")
      )
    )
  }

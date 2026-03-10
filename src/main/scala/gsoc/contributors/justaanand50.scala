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

// Must be at file scope — not inside an expression block
private case class WpmState(
    promptIdx: Int,
    typed: String,
    startMs: Option[Long],
    nowMs: Long,
    done: Boolean
)

val justaanand50: Contributor = Contributor("justaanand50"):

  val prompts = Vector(
    "a monad is just a monoid in the category of endofunctors",
    "pure functions always return the same output for the same input",
    "referential transparency makes code easier to reason about",
    "cats effect makes side effects safe and composable in scala",
    "type safety catches bugs at compile time before they reach production",
    "with great types comes great compile time guarantees"
  )

  def getPrompt(s: WpmState): String = prompts(s.promptIdx % prompts.length)

  def fresh(idx: Int, now: Long): WpmState =
    WpmState(idx, "", None, now, false)

  def calcWpm(chars: Int, startMs: Long, nowMs: Long): Int =
    val ms = math.max(1L, nowMs - startMs)
    math.round(chars / 5.0 / (ms / 60000.0)).toInt

  def calcAcc(p: String, typed: String): Int =
    if typed.isEmpty then 100
    else typed.zip(p).count(_ == _) * 100 / typed.length

  // Renders each character of the prompt coloured by correctness.
  // Returns Resource[IO, HtmlElement[IO]] so it can be used as
  // Signal[IO, Resource[IO, HtmlElement[IO]]] via state.map(s => renderPrompt(...))
  def renderPrompt(p: String, typed: String): Resource[IO, HtmlElement[IO]] =
    div(
      styleAttr := "font-family:monospace;font-size:0.95em;line-height:2.2;padding:12px;background:#1e293b;border-radius:6px;border:1px solid #334155;margin:8px 0;word-break:break-all;",
      p.zipWithIndex.toList.map { case (ch, i) =>
        val s =
          if i < typed.length then
            if typed(i) == ch then "color:#34d399;"
            else "background:#450a0a;color:#fca5a5;"
          else if i == typed.length then
            "color:#e2e8f0;border-bottom:2px solid #34d399;"
          else "color:#475569;"
        span(styleAttr := s, if ch == ' ' then "\u00a0" else ch.toString)
      }
    )

  // Initialise state with real clock time so WPM is accurate from tick 1
  IO.realTime.map(_.toMillis).flatMap(now =>
    SignallingRef[IO].of(fresh(0, now))
  ).toResource.flatMap { state =>

    // Background ticker: refreshes nowMs every 500 ms so live WPM updates
    val ticker: IO[Unit] =
      (IO.realTime.map(_.toMillis).flatMap(now =>
        state.update(s =>
          if s.startMs.isDefined && !s.done then s.copy(nowMs = now) else s
        )
      ) >> IO.sleep(500.millis)).foreverM

    ticker.background >> div(

      styleAttr := "font-family:'Segoe UI',system-ui,sans-serif;background:#0f172a;color:#e2e8f0;padding:20px;border-radius:12px;max-width:500px;border:1px solid #1e293b;box-shadow:0 8px 24px rgba(0,0,0,0.6);",

      // Identity + CoC
      p(
        styleAttr := "margin:0 0 2px 0;font-size:0.85em;color:#94a3b8;",
        "Hello, I'm ",
        span(styleAttr := "color:#34d399;font-weight:700;", "@justaanand50"),
        " on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."
      ),

      h3(
        styleAttr := "margin:10px 0 2px 0;font-size:1em;color:#34d399;letter-spacing:1px;",
        "⌨ Typing Speed Test"
      ),

      p(
        styleAttr := "margin:0 0 4px 0;font-size:0.78em;color:#64748b;",
        "Type the prompt. WPM and accuracy update live. Green = correct, red = wrong."
      ),

      // Per-character highlighted prompt — re-renders on every state change
      state.map(s => renderPrompt(getPrompt(s), s.typed)),

      // Live stats bar
      state.map { s =>
        val p   = getPrompt(s)
        val wpm = s.startMs.fold("—")(t => calcWpm(s.typed.length, t, s.nowMs).toString)
        val acc = calcAcc(p, s.typed)
        div(
          styleAttr := "display:flex;gap:20px;margin:6px 0;font-size:0.82em;color:#94a3b8;",
          span(
            "WPM ",
            span(styleAttr := "color:#34d399;font-weight:700;font-size:1.15em;", wpm)
          ),
          span(
            "Acc ",
            span(
              styleAttr := s"font-weight:700;color:${if acc >= 90 then "#34d399" else "#f87171"};",
              s"$acc%"
            )
          ),
          span(s"${math.min(s.typed.length, p.length)} / ${p.length} chars")
        )
      },

      // Completion banner — only visible when done
      state.map { s =>
        if s.done then
          div(
            styleAttr := "padding:10px 14px;background:#052e16;border:1px solid #166534;border-radius:6px;margin:6px 0;text-align:center;",
            span(
              styleAttr := "color:#34d399;font-weight:700;",
              s"✓  ${s.startMs.fold("—")(t => calcWpm(s.typed.length, t, s.nowMs).toString)} WPM  ·  ${calcAcc(getPrompt(s), s.typed)}% accuracy"
            )
          )
        else div(styleAttr := "display:none;", "")
      },

      // Controlled text input — always rendered, disabled when done
      input.withSelf { self =>
        (
          typ := "text",
          placeholder := "Start typing here…",
          styleAttr := "width:100%;padding:10px 12px;background:#1e293b;color:#e2e8f0;border:1px solid #334155;border-radius:6px;font-family:monospace;font-size:0.95em;outline:none;box-sizing:border-box;margin-top:4px;",
          disabled <-- state.map(_.done),
          value <-- state.map(_.typed),
          onInput --> (_.foreach { _ =>
            self.value.get.flatMap { v =>
              IO.realTime.map(_.toMillis).flatMap { now =>
                state.update { s =>
                  val p       = getPrompt(s)
                  val trimmed = v.take(p.length)
                  val started = s.startMs.orElse(Option.when(trimmed.nonEmpty)(now))
                  val done    = trimmed.length == p.length
                  s.copy(typed = trimmed, startMs = started, nowMs = now, done = done)
                }
              }
            }
          })
        )
      },

      // Next prompt / Reset
      div(
        styleAttr := "margin-top:10px;display:flex;gap:6px;",
        button(
          styleAttr := "padding:5px 14px;border-radius:6px;border:1px solid #334155;cursor:pointer;font-size:0.83em;background:#1e293b;color:#e2e8f0;",
          onClick --> (_.foreach { _ =>
            IO.realTime.map(_.toMillis).flatMap(now =>
              state.update(s => fresh(s.promptIdx + 1, now))
            )
          }),
          "Next prompt"
        ),
        button(
          styleAttr := "padding:5px 14px;border-radius:6px;border:1px solid #334155;cursor:pointer;font-size:0.83em;background:#1e293b;color:#e2e8f0;",
          onClick --> (_.foreach { _ =>
            IO.realTime.map(_.toMillis).flatMap(now =>
              state.update(s => fresh(s.promptIdx, now))
            )
          }),
          "Reset"
        )
      )
    )
  }
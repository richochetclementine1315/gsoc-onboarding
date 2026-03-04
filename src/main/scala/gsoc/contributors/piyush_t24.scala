package gsoc
package contributors

import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.syntax.*
import scala.concurrent.duration.DurationInt
import scala.util.Random

val `piyush-t24`: Contributor =
  Contributor("piyush-t24"):
    // domain model
    final case class Card(
        id: Int,
        emoji: String,
        isRevealed: Boolean,
        isMatched: Boolean
    )

    final case class GameState(
        cards: Vector[Card],
        firstSelected: Option[Int],
        lockBoard: Boolean,
        moves: Int,
        won: Boolean
    )

    def freshState: IO[GameState] = IO {
      val emojis =
        Vector("🐱", "🐶", "🦊", "🐼", "🐸", "🐵", "🦁", "🐷")
      val pairs = emojis.flatMap(e => List(e, e)) // 16 cards, 8 pairs

      val shuffled = Random
        .shuffle(pairs.zipWithIndex)
        .zipWithIndex
        .map {
          case ((emoji, _origIdx), id) =>
            Card(id = id, emoji = emoji, isRevealed = false, isMatched = false)
        }
        .toVector

      GameState(
        cards = shuffled,
        firstSelected = None,
        lockBoard = false,
        moves = 0,
        won = false
      )
    }

    def flipBack(state: SignallingRef[IO, GameState], i1: Int, i2: Int): IO[Unit] =
      IO.sleep(1.second) >>
        state.update { st =>
          // if indices are still in range, flip them back
          if i1 >= 0 && i1 < st.cards.length && i2 >= 0 && i2 < st.cards.length then
            val c1 = st.cards(i1)
            val c2 = st.cards(i2)
            val updated =
              st.cards
                .updated(i1, c1.copy(isRevealed = false))
                .updated(i2, c2.copy(isRevealed = false))
            st.copy(cards = updated, lockBoard = false)
          else st.copy(lockBoard = false)
        }

    def handleCardClick(state: SignallingRef[IO, GameState], idx: Int): IO[Unit] =
      state.get.flatMap { s =>
        if s.lockBoard || s.won then IO.unit
        else
          s.cards.lift(idx) match
            case None => IO.unit
            case Some(card) if card.isMatched || card.isRevealed =>
              IO.unit
            case Some(card) =>
              s.firstSelected match
                case None =>
                  // first selection
                  val updatedCards =
                    s.cards.updated(idx, card.copy(isRevealed = true))
                  state.set(
                    s.copy(
                      cards = updatedCards,
                      firstSelected = Some(idx)
                    )
                  )

                case Some(firstIdx) if firstIdx == idx =>
                  // clicking the same card again does nothing
                  IO.unit

                case Some(firstIdx) =>
                  val firstCard = s.cards(firstIdx)
                  val flippedBoth =
                    s.cards
                      .updated(firstIdx, firstCard.copy(isRevealed = true))
                      .updated(idx, card.copy(isRevealed = true))
                  val moves1 = s.moves + 1
                  val isMatch = firstCard.emoji == card.emoji

                  if isMatch then
                    val matched =
                      flippedBoth
                        .updated(
                          firstIdx,
                          firstCard.copy(isRevealed = true, isMatched = true)
                        )
                        .updated(
                          idx,
                          card.copy(isRevealed = true, isMatched = true)
                        )
                    val wonNow = matched.forall(_.isMatched)
                    state.set(
                      s.copy(
                        cards = matched,
                        firstSelected = None,
                        lockBoard = false,
                        moves = moves1,
                        won = wonNow
                      )
                    )
                  else
                    // mismatch: reveal, lock board, then flip back after delay
                    val locked =
                      s.copy(
                        cards = flippedBoth,
                        firstSelected = None,
                        lockBoard = true,
                        moves = moves1
                      )
                    state.set(locked) >>
                      flipBack(state, firstIdx, idx).start.void
      }

    def resetGame(state: SignallingRef[IO, GameState]): IO[Unit] =
      freshState.flatMap(state.set)

    // build resource with reactive state
    Resource.eval(freshState).flatMap { initial =>
      SignallingRef[IO].of(initial).toResource.flatMap { state =>
        div(
          styleAttr :=
            """
            padding: 28px;
            margin: 16px 0;
            border-radius: 16px;
            background: #020617;
            color: #e5e7eb;
            font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
            border: 1px solid #1e293b;
            box-shadow: 0 4px 20px rgba(0, 0, 0, 0.4);
            """,
          // Top: intro + CoC / GSoC AI statement (README style)
          p(
            styleAttr := "margin: 0 0 20px 0; font-size: 0.95rem; line-height: 1.5; color: #e5e7eb; text-align: center;",
            "Hello, I'm ",
            a(
              href := "https://github.com/Piyush-t24",
              target := "_blank",
              rel := List("noopener", "noreferrer"),
              styleAttr := "font-weight: bold; color: #ffffff; text-decoration: none;",
              "@Piyush-t24"
            ),
            " on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."
          ),
          // Middle: game section in a box
          div(
            styleAttr :=
              """
              padding: 20px;
              border-radius: 12px;
              background: #0f172a;
              border: 1px solid #334155;
              """,
            h2(
              styleAttr := "margin: 0 0 16px 0; font-size: 1.25rem; color: #ffffff; text-align: center;",
              "Memory Match"
            ),
            state.map { s =>
              val statusText =
                if s.won then s"🎉 You won in ${s.moves} moves!"
                else if s.lockBoard then "Checking cards..."
                else "Find all matching pairs!"

              div(
                // status + moves
                div(
                  styleAttr :=
                    "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
                  calico
                    .html
                    .io
                    .span(
                      styleAttr := "font-size: 0.9rem; color: #9ca3af;",
                      statusText
                    ),
                  calico
                    .html
                    .io
                    .span(
                      styleAttr := "font-size: 0.9rem; color: #e5e7eb;",
                      s"Moves: ${s.moves}"
                    )
                ),
                // reset button
                div(
                  styleAttr := "display: flex; justify-content: center; margin-bottom: 16px;",
                  button(
                    styleAttr :=
                      """
                      padding: 6px 12px;
                      border-radius: 999px;
                      border: none;
                      cursor: pointer;
                      font-size: 0.85rem;
                      background: #22c55e;
                      color: #022c22;
                      """,
                    "Reset game",
                    onClick --> (_.foreach(_ => resetGame(state)))
                  )
                ),
                // grid
                div(
                  styleAttr :=
                    """
                    display: grid;
                    grid-template-columns: repeat(4, minmax(0, 1fr));
                    gap: 8px;
                    """,
                  s.cards.zipWithIndex.toList.map {
                    case (card, idx) =>
                      val showEmoji = card.isRevealed || card.isMatched
                      val bgColor =
                        if card.isMatched then "#16a34a"
                        else if card.isRevealed then "#0ea5e9"
                        else "#111827"
                      val borderColor =
                        if s.firstSelected.contains(idx) then "#eab308"
                        else "#1f2937"

                      val disabledNow =
                        card.isMatched || s.lockBoard || s.won

                      val label = if showEmoji then card.emoji else "❓"

                      button(
                        styleAttr :=
                          s"""
                        height: 56px;
                        border-radius: 12px;
                        border: 1px solid $borderColor;
                        background: $bgColor;
                        color: #f9fafb;
                        font-size: 1.6rem;
                        display: flex;
                        align-items: center;
                        justify-content: center;
                        cursor: ${
                              if disabledNow then "default" else "pointer"
                            };
                        transition: transform 0.1s ease, box-shadow 0.1s ease, background 0.1s ease;
                        box-shadow: ${
                              if card.isRevealed || card.isMatched then
                                "0 4px 10px rgba(0,0,0,0.4)"
                              else "none"
                            };
                        """,
                        disabled := disabledNow,
                        onClick --> (_.foreach(_ => handleCardClick(state, idx))),
                        label
                      )
                  }
                )
              )
            }
          )
        )
      }
    }

package gsoc
package contributors

import cats.effect.*
import fs2.concurrent.*
import calico.html.io.{*, given}
import calico.syntax.*

val richochetclementine1315: Contributor = Contributor("richochetclementine1315"):
  SignallingRef[IO].of(0).toResource.flatMap { idx =>

    val emojis = Vector("😀", "🎃", "🐶", "🦄", "🍕", "🚀", "🌈", "💎", "🔥", "⭐")

    div(
      p(
    "Hello, I'm ",
    span(
      "@richochetclementine1315",
      styleAttr := "color: #FF5733; font-weight: bold;"
    ),
     span(" on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy.", styleAttr := "color: #FF5733; font-weight: bold;")
      ),
      h2("Click the emoji to change it!"),
      div(
        styleAttr := "font-size: 6em; cursor: pointer; user-select: none;",
        onClick --> (_.foreach(_ => idx.update(i => (i + 1) % emojis.length))),
        calico.html.io.span(idx.map(i => emojis(i)))
      )
    )
  }
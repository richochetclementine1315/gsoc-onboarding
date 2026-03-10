package gsoc
package contributors

import cats.effect.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.syntax.*

val ankitkumarrain: Contributor = Contributor("ankitkumarrain"):
  SignallingRef[IO].of(false).toResource.flatMap { clicked =>

    div(
      p("I love open-source projects and looking forward to excel with Typelevel Projects and their Maintainers"),

      button(
        styleAttr := "background-color: rgb(5, 107, 48); color: white; border: none; padding: 10px 20px; border-radius: 5px; cursor: pointer;",
        "lets connect ?",
        onClick --> (_.foreach(_ => clicked.set(true)))
      ),
      clicked.map {
        case true =>
          div(
            p("My github username name is ankitkumarain, we can connect there"),
            p("I want to work on fs2 filesystem bridge")
          )

        case false =>
          div("")
      }


    )
  }

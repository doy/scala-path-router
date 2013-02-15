import org.scalatest.FunSuite

import router.Router

class Optional extends FunSuite with RouterHelpers {
  val router = new Router[Boolean]

  router addRoute (
    ":controller/?:action",
    true,
    defaults = Map(
      "action" -> "index"
    ),
    validations = Map(
      "action" -> """\D+""".r
    )
  )

  router addRoute (
    ":controller/:id/?:action",
    true,
    defaults = Map(
      "action" -> "show"
    ),
    validations = Map(
      "id" -> """\d+""".r
    )
  )

  test ("routes match properly") {
    testRoute(
      router, "people", Map(
        "controller" -> "people",
        "action"     -> "index"
      )
    )

    testRoute(
      router, "people/new", Map(
        "controller" -> "people",
        "action"     -> "new"
      )
    )

    testRoute(
      router, "people/create", Map(
        "controller" -> "people",
        "action"     -> "create"
      )
    )

    testRoute(
      router, "people/56", Map(
        "controller" -> "people",
        "action"     -> "show",
        "id"         -> "56"
      )
    )

    testRoute(
      router, "people/56/edit", Map(
        "controller" -> "people",
        "action"     -> "edit",
        "id"         -> "56"
      )
    )

    testRoute(
      router, "people/56/remove", Map(
        "controller" -> "people",
        "action"     -> "remove",
        "id"         -> "56"
      )
    )

    testRoute(
      router, "people/56/update", Map(
        "controller" -> "people",
        "action"     -> "update",
        "id"         -> "56"
      )
    )
  }
}

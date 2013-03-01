package org.perl8.router

import org.scalatest.FunSuite

import org.perl8.router.test._

class OptionalTest extends FunSuite {
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
    assert(
      router matches "people", Map(
        "controller" -> "people",
        "action"     -> "index"
      )
    )

    assert(
      router matches "people/new", Map(
        "controller" -> "people",
        "action"     -> "new"
      )
    )

    assert(
      router matches "people/create", Map(
        "controller" -> "people",
        "action"     -> "create"
      )
    )

    assert(
      router matches "people/56", Map(
        "controller" -> "people",
        "action"     -> "show",
        "id"         -> "56"
      )
    )

    assert(
      router matches "people/56/edit", Map(
        "controller" -> "people",
        "action"     -> "edit",
        "id"         -> "56"
      )
    )

    assert(
      router matches "people/56/remove", Map(
        "controller" -> "people",
        "action"     -> "remove",
        "id"         -> "56"
      )
    )

    assert(
      router matches "people/56/update", Map(
        "controller" -> "people",
        "action"     -> "update",
        "id"         -> "56"
      )
    )
  }
}

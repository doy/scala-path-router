package org.perl8.router

import org.scalatest.FunSuite

import org.perl8.router.test._

class BasicTest extends FunSuite {
  val yearRx  = """\d{4}""".r
  val monthRx = """\d|10|11|12""".r
  val dayRx   = """\d|[12]\d|30|31""".r

  val router = new Router[Boolean]

  router addRoute (
    "blog",
    true,
    defaults = Map(
      "controller" -> "blog",
      "action"     -> "index"
    )
  )

  router addRoute (
    "blog/:year/:month/:day",
    true,
    defaults = Map(
      "controller" -> "blog",
      "action"     -> "show_date"
    ),
    validations = Map(
      "year"  -> yearRx,
      "month" -> monthRx,
      "day"   -> dayRx
    )
  )

  router addRoute (
    "blog/:action/:id",
    true,
    defaults = Map(
      "controller" -> "blog"
    ),
    validations = Map(
      "action" -> """\D+""".r,
      "id"     -> """\d+""".r
    )
  )

  router addRoute (
    "test/?:x/?:y",
    true,
    defaults = Map(
      "controller" -> "test",
      "x"          -> "x",
      "y"          -> "y"
    )
  )

  test ("routes match properly") {
    assert(
      router matches "blog", Map(
        "controller" -> "blog",
        "action"     -> "index"
      )
    )

    assert(
      router matches "blog/2006/12/5", Map(
        "controller" -> "blog",
        "action"     -> "show_date",
        "year"       -> "2006",
        "month"      -> "12",
        "day"        -> "5"
      )
    )

    assert(
      router matches "blog/1920/12/10", Map(
        "controller" -> "blog",
        "action"     -> "show_date",
        "year"       -> "1920",
        "month"      -> "12",
        "day"        -> "10"
      )
    )

    assert(
      router matches "blog/edit/5", Map(
        "controller" -> "blog",
        "action"     -> "edit",
        "id"         -> "5"
      )
    )

    assert(
      router matches "blog/show/123", Map(
        "controller" -> "blog",
        "action"     -> "show",
        "id"         -> "123"
      )
    )

    assert(
      router matches "blog/some_crazy_long_winded_action_name/12356789101112131151", Map(
        "controller" -> "blog",
        "action"     -> "some_crazy_long_winded_action_name",
        "id"         -> "12356789101112131151"
      )
    )

    assert(
      router matches "blog/delete/5", Map(
        "controller" -> "blog",
        "action"     -> "delete",
        "id"         -> "5"
      )
    )

    assert(
      router matches "test/x1", Map(
        "controller" -> "test",
        "x"          -> "x1",
        "y"          -> "y"
      )
    )
  }
}

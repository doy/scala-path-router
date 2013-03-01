package org.perl8.router

import org.scalatest.FunSuite

import org.perl8.router.test._

class MessyTest extends FunSuite {
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
      "year"  -> """\d{4}""".r,
      "month" -> """\d{1,2}""".r,
      "day"   -> """\d{1,2}""".r
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

  test ("our routes match") {
    assert(router matches "/blog/")
    assert(router matches "./blog/")
    assert(router matches "///.///.///blog//.//")
    assert(router matches "/blog/./show/.//./20")
    assert(router matches "/blog/./2006/.//./20////////10")
  }
}

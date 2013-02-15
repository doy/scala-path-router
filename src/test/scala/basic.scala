import org.scalatest.FunSuite

import router.Router

class Basic extends FunSuite {
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

  def testRoute (router: Router[Boolean], path: String, mapping: Map[String, String]) {
    assert(path === router.uriFor(mapping).get)
    val om = router.route(path)
    assert(om.isDefined)
    val m = om.get
    assert(m.mapping.size == mapping.size)
    assert(m.mapping.forall { case (k, v) => mapping(k) == v })
    assert(m.target === true)
  }

  test ("routes match properly") {
    testRoute(
      router, "blog", Map(
        "controller" -> "blog",
        "action"     -> "index"
      )
    )

    testRoute(
      router, "blog/2006/12/5", Map(
        "controller" -> "blog",
        "action"     -> "show_date",
        "year"       -> "2006",
        "month"      -> "12",
        "day"        -> "5"
      )
    )

    testRoute(
      router, "blog/1920/12/10", Map(
        "controller" -> "blog",
        "action"     -> "show_date",
        "year"       -> "1920",
        "month"      -> "12",
        "day"        -> "10"
      )
    )

    testRoute(
      router, "blog/edit/5", Map(
        "controller" -> "blog",
        "action"     -> "edit",
        "id"         -> "5"
      )
    )

    testRoute(
      router, "blog/show/123", Map(
        "controller" -> "blog",
        "action"     -> "show",
        "id"         -> "123"
      )
    )

    testRoute(
      router, "blog/some_crazy_long_winded_action_name/12356789101112131151", Map(
        "controller" -> "blog",
        "action"     -> "some_crazy_long_winded_action_name",
        "id"         -> "12356789101112131151"
      )
    )

    testRoute(
      router, "blog/delete/5", Map(
        "controller" -> "blog",
        "action"     -> "delete",
        "id"         -> "5"
      )
    )

    testRoute(
      router, "test/x1", Map(
        "controller" -> "test",
        "x"          -> "x1",
        "y"          -> "y"
      )
    )
  }
}

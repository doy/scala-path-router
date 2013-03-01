package org.perl8.router

object test {
  import language.implicitConversions

  class RouterHelperOps[T] (router: Router[T]) {
    private def assert (condition: Boolean, msg: String) = {
      if (condition) None else Some(msg)
    }

    def matches (path: String) = {
      assert(router.route(path).isDefined, s"route failed to match $path")
    }

    def matches (path: String, mapping: Map[String, String]) = {
      (router.uriFor(mapping), router.route(path)) match {
        case (Some(uriFor), Some(m)) => {
          None.orElse({
            assert(uriFor == path, s"uriFor returned $uriFor, expected $path")
          }).orElse({
            assert(
              m.mapping.size == mapping.size &&
                m.mapping.forall { case (k, v) => mapping(k) == v },
              s"route returned ${m.mapping}, expected $mapping"
            )
          })
        }
        case (None, None) =>
          Some(s"uriFor and route both failed to match $path")
        case (None, _) =>
          Some(s"uriFor failed to match $path")
        case (_, None) =>
          Some(s"route failed to match $path")
      }
    }
  }

  implicit def routerToOps[T] (router: Router[T]): RouterHelperOps[T] =
    new RouterHelperOps(router)
}

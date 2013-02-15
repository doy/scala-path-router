import org.scalatest.FunSuite

import router.Router

trait RouterHelpers { this: FunSuite =>
  def testRoute (router: Router[Boolean], path: String, mapping: Map[String, String]) {
    assert(path === router.uriFor(mapping).get)
    val om = router.route(path)
    assert(om.isDefined)
    val m = om.get
    assert(m.mapping.size == mapping.size)
    assert(m.mapping.forall { case (k, v) => mapping(k) == v })
    assert(m.target === true)
  }
}

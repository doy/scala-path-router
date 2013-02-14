package router

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class Router[T] {
  val routes = new ArrayBuffer[Route[T]]()

  def addRoute (
    path:        String,
    target:      T,
    defaults:    Map[String, String] = Map(),
    validations: Map[String, Regex]  = Map()
  ) {
    routes += new Route(path, defaults, validations, target)
  }

  def insertRoute (
    path:        String,
    target:      T,
    defaults:    Map[String, String] = Map(),
    validations: Map[String, Regex]  = Map(),
    at:          Int                 = 0
  ) {
    routes insert (
      at min routes.length,
      new Route(path, defaults, validations, target)
    )
  }

  def route (path: String): Option[Match[T]] = {
    def _route (
      components: Seq[String],
      routes:     List[Route[T]]
    ): Option[Match[T]] = routes match {
      case r :: rs => r.route(components) match {
        case Some(found) => Some(found)
        case None        => _route(components, rs)
      }
      case _ => None
    }
    _route(path.split("/"), routes.toList)
  }

  def uriFor (mapping: Map[String, String]): String = {
    throw new Error("unimplemented")
  }
}

class Route[T] (
  val path:        String,
  val defaults:    Map[String, String],
  val validations: Map[String, Regex],
  val target:      T
) {
  def route(
    parts:      Seq[String],
    components: Seq[String]         = components,
    mapping:    Map[String, String] = defaults
  ): Option[Match[T]] = {
    if (components.filter(!isOptional(_)).length == 0 && parts.length == 0) {
      Some(new Match[T](path, mapping, target))
    }
    else if (components.length == 0 || parts.length == 0) {
      None
    }
    else {
      components.head match {
        case Optional(name) => {
          if (validate(name, parts.head)) {
            route(parts.tail, components.tail, mapping + (name -> parts.head))
          }
          else {
            route(parts, components.tail, mapping)
          }
        }
        case Variable(name) => {
          if (validate(name, parts.head)) {
            route(parts.tail, components.tail, mapping + (name -> parts.head))
          }
          else {
            None
          }
        }
        case literal => parts.head match {
          case `literal` => route(parts.tail, components.tail, mapping)
          case _         => None
        }
      }
    }
  }

  override def toString = path

  private lazy val components =
    path.split("/").filter(_.length > 0)

  lazy val requiredVariableComponents =
    components.filter(!isOptional(_)).flatMap(getComponentName)

  lazy val optionalVariableComponents =
    components.filter(isOptional).flatMap(getComponentName)

  private val Optional = """^\?:(.*)$""".r
  private val Variable = """^\??:(.*)$""".r

  private def isOptional (component: String): Boolean = component match {
    case Optional(_) => true
    case _           => false
  }

  private def isVariable (component: String): Boolean = component match {
    case Variable(_) => true
    case _           => false
  }

  private def getComponentName (component: String) = component match {
    case Variable(name) => Some(name)
    case _              => None
  }

  private def validate (name: String, component: String): Boolean =
    validations get name match {
      case Some(rx) => rx.findFirstIn(component).nonEmpty
      case None     => true
    }
}

class Match[T] (
  val path:    String,
  val mapping: Map[String, String],
  val target:  T
)

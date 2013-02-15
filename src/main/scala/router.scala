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
    def testRoutes (
      components: Seq[String],
      routes:     List[Route[T]]
    ): Option[Match[T]] = routes match {
      case r :: rs => r.route(components) match {
        case Some(found) => Some(found)
        case None        => testRoutes(components, rs)
      }
      case _ => None
    }
    testRoutes(path.split("/"), routes.toList)
  }

  private class AmbiguousRouteMapping(
    mapping: Map[String, String],
    paths:   Seq[String]
  ) extends RuntimeException {
    override def getMessage (): String = {
      "Ambiguous path descriptor (specified keys " +
      mapping.keys.mkString(", ")                  +
      "): could match paths "                      +
      paths.mkString(", ")
    }
  }

  def uriFor (mapping: Map[String, String]): Option[String] = {
    // first remove all routes that can't possibly match
    // - if the route requires a variable component that doesn't exist in the
    //   mapping, then it can't match
    // - if the route contains a value for a variable component that doesn't
    //   pass the validation for that component, it can't match
    // - if the route contains a default value, and that component also exists
    //   in the mapping, then the values must match
    val possible = routes.flatMap(r => {
      r.pathWithMapping(mapping) match {
        case Some(path) => Some(r -> path)
        case None       => None
      }
    })

    possible.toList match {
      case Nil              => None
      case (r, path) :: Nil => Some(path)
      case rs               => {
        // then try to disambiguate the remaining possibilities
        // - we want the route with the fewest number of "extra" items in the
        //   mapping, after removing defaults and variable path components
        val possibleByRemainder = possible.groupBy { case (r, path) => {
          (mapping.keys.toSet -- r.defaults.keys.toSet -- r.variables).size
        } }
        val found = possibleByRemainder(possibleByRemainder.keys.min)
        found.toList match {
          case Nil              => None
          case (r, path) :: Nil => Some(path)
          case rs               =>
            throw new AmbiguousRouteMapping(mapping, rs.map(_._1.path))
        }
      }
    }
  }
}

class Route[T] (
  val path:        String,
  val defaults:    Map[String, String],
  val validations: Map[String, Regex],
  val target:      T
) {
  import Route._

  lazy val variables = components.flatMap(getVariableName)

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

  def pathWithMapping (mapping: Map[String, String]): Option[String] = {
    val requiredDefaults = defaults.keys.filter { k =>
      mapping.isDefinedAt(k) && !variables.contains(k)
    }
    if (requiredDefaults.forall(k => defaults(k) == mapping(k)) &&
        requiredVariables.forall(mapping.isDefinedAt)) {
      val boundComponents = components.flatMap {
        case Optional(v) => {
          val component = (mapping get v).flatMap(validComponentValue(v, _))
          defaults get v match {
            case Some(default) => {
              component.flatMap {
                case `default` => None
                case c         => Some(c)
              }
            }
            case None => component
          }
        }
        case Variable(v) => validComponentValue(v, (mapping(v)))
        case literal     => Some(literal)
      }
      Some(boundComponents.mkString("/"))
    }
    else {
      None
    }
  }

  private lazy val components =
    path.split("/").filter(_.length > 0)

  private lazy val requiredVariables =
    components.filter(!isOptional(_)).flatMap(getVariableName)

  private lazy val hasVariable = variables.toSet

  private def isOptional (component: String) =
    Optional.findFirstIn(component).nonEmpty

  private def isVariable (component: String) =
    Variable.findFirstIn(component).nonEmpty

  private def getVariableName (component: String) = component match {
    case Variable(name) => Some(name)
    case _              => None
  }

  private def validate (name: String, component: String) =
    validations get name match {
      case Some(rx) => rx.findFirstIn(component).nonEmpty
      case None     => true
    }

  private def validComponentValue (name: String, component: String) =
    if (validate(name, component)) { Some(component) } else { None }
}

object Route {
  private val Optional = """^\?:(.*)$""".r
  private val Variable = """^\??:(.*)$""".r
}

class Match[T] (
  val path:    String,
  val mapping: Map[String, String],
  val target:  T
)

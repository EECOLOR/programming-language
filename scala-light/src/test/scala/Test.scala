

import syntax.Parser
import scala.io.Source
import java.io.File
import fastparse.core.Parsed
import syntax.ast.Statement
import org.anormcypher._
import play.api.libs.ws._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import java.util.UUID
import syntax.ast.Position
import syntax.ast.AstNode
import syntax.UsefulDataTypes
import syntax.ast.Expression
import syntax.ast.Shared

object Test {
  val file = new File("/home/eecolor/eecolor/programming-language/scala-light/src/main/scala/syntax/Parser.scala")
  val code = Source.fromFile(file).getLines.mkString("\n")
  val result = Parser.statements.`package`.parse(code)

  def test = {
    println(result)
    result match {
      case Parsed.Success(value, _) =>
        process(value)
      case Parsed.Failure(_, _, extra) =>
        println(extra.traced.trace)
    }
  }

  import Statement._
  import Expression._
  import Shared._
  import UsefulDataTypes._

  case class Result(value: Future[Seq[CypherResultRow]])

  implicit class AsResult(c: CypherStatement)(implicit tx: Neo4jTransaction, ec: ExecutionContext) {
    def asResult = Result(c.async())
  }

  case class Prepared(statement: String, arguments: Map[String, Any], nodes: Set[String]) {
    def + (other: Prepared) =
      Prepared(statement + other.statement, arguments ++ other.arguments, nodes ++ other.nodes)
  }
  case object N

  def newId: String = "`" + UUID.randomUUID.toString + "`"

  implicit class CypherHelper(val sc: StringContext) extends AnyVal {
    def c(args: Any*): Prepared = {
      val i = args.iterator
      val Seq(first, rest @ _*) = sc.parts
      val start = Prepared(first, Map.empty, Set.empty)
      rest.foldLeft(start) {
        case (Prepared(statement, arguments, nodes), part) =>
          i.next match {
            case id: String =>
              Prepared(statement + id + part, arguments, nodes + id)
            case m: Map[String, Any] =>
              val id = newId
              Prepared(s"$statement{ $id }$part", arguments + (id -> m), nodes)
          }
      }
    }
  }

  def process(p: Package) = {
    implicit val wsclient = ning.NingWSClient()
    try {
      val connection: Neo4jConnection = Neo4jREST("localhost", 7474, "neo4j", "neo4jj")

      Cypher

    } finally wsclient.close()

  }

  class Concat[-T](replace: (Group, T) => Group) {
    def apply(g: Group, i: T) = replace(g, i)
  }
  object Concat {
    implicit object Node     extends Concat[Iterable[Node]]    ((n, i) => n.copy(others    = n.others    ++ i))
    implicit object Group    extends Concat[Iterable[Group]]   ((n, i) => n.copy(children  = n.children  ++ i))
    implicit object Relation extends Concat[Iterable[Relation]]((n, i) => n.copy(relations = n.relations ++ i))
  }

  case class Node(
    id: String,
    label: String,
    properties: Map[String, Any] = Map.empty
  ) {
    def asGroup = Group(this)
  }
  case class Relation(from: Node, to: Node, label: String, properties: Map[String, Any] = Map.empty)
  case class Group(
    node: Node,
    others: Set[Node] = Set.empty,
    children: Set[Group] = Set.empty,
    relations: Set[Relation] = Set.empty
  ) {
    def + (n: Node)     = copy(others    = others + n)
    def + (g: Group)    = copy(children  = children + g)
    def + (r: Relation) = copy(relations = relations + r)

    def ++ [T](n: T)(implicit x: Concat[T]) = x(this, n)

    def -< (children: Seq[Group]) = {
      val childRelations = children match {
        case Seq(first, rest @ _*) =>
          Set(this - "First" -> first) ++ chain(children, "Next")
        case _ => Set.empty
      }

      this ++ children ++ childRelations
    }
  }

  def asNode(v: Value): Node = Node(newId, "Value", v.pos + ("value" -> v.value))

  def propRel(from: Node, to: Node, prop: String) =
    Relation(from, to, "Property", Map("name" -> prop))

  implicit class NodeOps(n: Node) {
    case class - (r: (Node, Node) => Relation) {
      def -> (o: Node)  = r(n, o)
      def -> (g: Group) = r(n, g.node)
    }
  }
  implicit class GroupOps(g: Group) {
    case class - (r: (Node, Node) => Relation) {
      def -> (o: Node)  = r(g.node, o)
      def -> (o: Group) = r(g.node, o.node)
    }
  }
  implicit class RelationOps(g: Relation) {
    case class - (r: (Node, Node) => Relation) {
      def -> (o: Node)  = Set(g, r(g.to, o))
      def -> (o: Group) = Set(g, r(g.to, o.node))
    }
  }

  import language.implicitConversions
  implicit def stringToRelationBuilder(s: String): (Node, Node) => Relation =
    (from, to) => Relation(from, to, s)

  implicit def _1(x: Seq[Statement | Expression] ): Seq[Group] = x map (this process _.merge)
  implicit def _2(x: Seq[AstNode]                ): Seq[Group] = x map process
  implicit def _3(x: Option[AstNode]             ): Seq[Group] = x.toSeq

  def prop(name: String) = (from: Node, to: Node) => Relation(from, to, "Property", Map("name" -> name))

  def asNode(id: Id): Group = process(id.merge)
  def asNode(ref: Option[Shared.Reference]): Option[Group] =
    ???

  def process: AstNode => Group = {

    case x: Value =>
      asNode(x).asGroup

    case x @ LiteralGroup(literal, v) =>
      val node = Node(newId, "LiteralGroup", x.pos + ("literal" -> literal)).asGroup
      val value = asNode(v)

      node + value + (node - prop("value") -> value)

    case x @ Package(path, body) =>
      val node = Node(newId, "Package", x.pos).asGroup -< body

      val pathNodes = path.map(asNode)
      val pathRelation = pathNodes match {
        case Seq(first, rest @ _*) => Set(node - prop("path") -> first)
        case _                     => Set.empty
      }

      node ++ pathNodes ++ pathRelation ++ chain(pathNodes, label = "Next")

    case x @ Class(name, typeArguments, arguments, extensions, body) =>
      val node = Node(newId, "Class", x.pos).asGroup -< typeArguments -< arguments -< extensions -< body
      val nameNode = asNode(name)

      node + nameNode + (node - prop("name") -> nameNode)

    case x @ Object(name, typeArguments, extensions, body) =>
      val node = Node(newId, "Object", x.pos).asGroup -< typeArguments -< extensions -< body
      val nameNode = asNode(name)

      node + nameNode + (node - prop("name") -> nameNode)

    case x @ Trait(name, typeArguments, arguments, extensions, body) => ???
      val node = Node(newId, "Trait", x.pos).asGroup -< typeArguments -< arguments -< extensions -< body
      val nameNode = asNode(name)

      node + nameNode + (node - prop("name") -> nameNode)

    case x @ Def(name, typeArguments, arguments, tpe, body) =>
      val node = Node(newId, "Def", x.pos).asGroup -< typeArguments -< arguments -< tpe
      val (nameNode, bodyNode) = (asNode(name), process(body))

      node + nameNode + bodyNode + (node - prop("name") -> nameNode) + (node - "Child" -> bodyNode)

    case x @ Val(name, typeArguments, tpe, body) =>
      val node = Node(newId, "Val", x.pos).asGroup -< typeArguments -< tpe
      val (nameNode, bodyNode) = (asNode(name), process(body))

      node + nameNode + bodyNode + (node - prop("name") -> nameNode) + (node - "Child" -> bodyNode)

    case x @ TypeConstructor(name, typeArguments, body) =>
      val node = Node(newId, "TypeConstructor", x.pos).asGroup -< typeArguments
      val (nameNode, bodyNode) = (asNode(name), process(body))

      node + nameNode + bodyNode + (node - prop("name") -> nameNode) + (node - "Child" -> bodyNode)

    case x @ MemberExtraction(target, names, source) =>
      val node = Node(newId, "MemberExtraction", x.pos)
      val (targetNode, sourceNode) = (asNode(target), process(source))

      ???
  }

  def chain(nodes: Seq[Group], label: String) =
    nodes.sliding(2).foldLeft(Set.empty[Relation]) {
      case (result, Seq(prev, next)) => result + (prev - label -> next)
      case (result, _) => result
    }

  implicit class PosOps(p: AstNode) {
    def pos: Map[String, Any] = {
      val Position(start, end) = p.position
      Map("start" -> start, "end" -> end)
    }
  }
}

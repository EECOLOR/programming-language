

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
import syntax.UsefulDataTypes._
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

  case class Node(
    label: String,
    properties: Map[String, Any] = Map.empty,
    relations: Set[Relation] = Set.empty
  ) { self =>

    val id = newId

    case class - (r: Node => Relation) {
      def -> [A](o: A)(implicit ev: A => Node)  = self.copy(relations = self.relations + r(o))
    }

    def -< (children: Seq[Node]) = {
      children match {
        case Seq(first, rest @ _*) =>
          this - "First" -> children.reduceRight(_ - "Next" -> _)
        case _ => this
      }
    }
  }

  case class Relation(to: Node, label: String, properties: Map[String, Any] = Map.empty)


  import language.implicitConversions
  implicit def stringToRelationBuilder(s: String): Node => Relation =
    to => Relation(to, s)


  implicit def _0(x: AstNode):Node = process(x)
  implicit def _1(x: AstNode | AstNode ): AstNode = x.merge
  implicit def _2[A](x: Seq[A])(implicit ev: A => AstNode): Seq[Node] = x map ev
  implicit def _3[A](x: Option[A])(implicit ev: A => AstNode): Seq[Node] = x.toSeq
  implicit def _4[A](x: NonEmptySeq[A])(implicit ev: A => AstNode): NonEmptySeq[Node] = x
  implicit def _6[A](x: A)(implicit ev: A => AstNode): Node = toNode(x)
  implicit def _7(x: Shared.Reference): Node = toNode(x)

  def prop(name: String) = (to: Node) => Relation(to, "Property", Map("name" -> name))

  def toNode[A](a: A)(implicit ev: A => AstNode): Node = ev(a)

  def toNode(ref: Option[Shared.Reference]): Option[Node] = ref map toNode
  def toNode(ref: Shared.Reference): Node =
    ref.to.map(process).toSeq.reduceRight(_ - "Next" -> _)

  implicit class Chainable[A](a: A)(implicit ev: A => NonEmptySeq[Node]) {
    def asChain(f: (Node, Node) => Node) = a.toSeq reduceRight f
  }

  def process: AstNode => Node = {

    case x @ Value(value) =>
      Node("Value", x.pos + ("value" -> value))

    case x @ LiteralGroup(literal, value) => (
      Node("LiteralGroup", x.pos + ("literal" -> literal))
        - prop("value") -> value
    )

    case x @ Package(path, body) =>
      val node = Node("Package", x.pos) -< _2(body)

      path map toNode[Id] match {
        case NonEmptySeq.FromSeq(x) => node - prop("path") -> x.asChain(_ - "Next" -> _)
        case _                      => node
      }

    case x @ Class(name, typeArguments, arguments, extensions, body) => (
      Node("Class", x.pos)
        -< typeArguments
        -< arguments
        -< extensions
        -< body
        - prop("name") -> name
    )

    case x @ Object(name, typeArguments, extensions, body) => (
      Node("Object", x.pos)
        -< typeArguments
        -< extensions
        -< body
        - prop("name") -> name
    )

    case x @ Trait(name, typeArguments, arguments, extensions, body) => (
      Node("Trait", x.pos)
        -< typeArguments
        -< arguments
        -< extensions
        -< body
        - prop("name") -> name
    )

    case x @ Def(name, typeArguments, arguments, tpe, body) => (
      Node("Def", x.pos)
        -< typeArguments
        -< arguments
        -< tpe
        - prop("name") -> name
        - "Child"      -> body
    )

    case x @ Val(name, typeArguments, tpe, body) => (
      Node("Val", x.pos)
        -< typeArguments
        -< tpe
        - prop("name") -> name
        - "Child"      -> body
    )

    case x @ TypeConstructor(name, typeArguments, body) => (
      Node("TypeConstructor", x.pos)
        -< typeArguments
        - prop("name") -> name
        - "Child"      -> body
    )

    case x @ MemberExtraction(target, names, source) =>
      val node = (
        Node("MemberExtraction", x.pos)
          - prop("source") -> source
          - prop("names")  -> names.asChain(_ - "Next" -> _)
      )
      toNode(target)
        .map(node - prop("target") -> _)
        .getOrElse(node)

    case x @ Marked(mark, statement) => (
      Node("Marked", x.pos)
        - prop("mark")      -> mark
        - prop("statement") -> statement
    )

    case x @ Comment(value) => (
      Node("Comment", x.pos)
        - prop("value") -> value
    )

    case x @ Import(value) => (
      Node("Import", x.pos)
        - prop("import") -> value
    )

    case x @ Import.Single(path) => (
      Node("Import.Single", x.pos)
        - prop("path") -> path
    )

    case x @ Import.Multiple(path, parts) => (
      Node("Import.Multiple", x.pos)
        - prop("path")  -> path
        - prop("parts") -> parts.asChain(_ - "Next" -> _)
    )

    case x @ Import.Id(id) => (
      Node("Import.Id", x.pos)
        - prop("id") -> id
    )

    case x @ Import.As(original, newId) => (
      Node("Import.As", x.pos)
        - prop("original") -> original
        - prop("newId")    -> newId
    )
  }

  implicit class PosOps(p: AstNode) {
    def pos: Map[String, Any] = {
      val Position(start, end) = p.position
      Map("start" -> start, "end" -> end)
    }
  }
}

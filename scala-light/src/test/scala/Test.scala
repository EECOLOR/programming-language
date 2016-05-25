

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
import play.api.libs.json.Json

object Test {
  val file = new File("/home/eecolor/eecolor/programming-language/scala-light/src/main/scala/syntax/Parser.scala")
  val code = Source.fromFile(file).getLines.mkString("\n")
  val result = Parser.statements.`package`.parse(code)

  def test = {
    println(result)
    result match {
      case Parsed.Success(value, _) =>
        store(value)
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

  case class Prepared(statement: String, parameters: Map[String, Map[String, Any]]) {
    def + (other: Prepared): Prepared =
      if (other == Prepared.empty) this
      else if (this == Prepared.empty) other
      else Prepared(statement + "\n" + other.statement, parameters ++ other.parameters)

    def asCypherStatement = CypherStatement(statement, parameters)
  }
  object Prepared {
    val empty: Prepared = Prepared("", Map.empty)
  }

  def newId: String = UUID.randomUUID.toString

  def store(p: Package) = {
    implicit val wsclient = ning.NingWSClient()
    try {
      implicit val connection: Neo4jConnection = Neo4jREST("localhost", 7474, "neo4j", "neo4jj")
      import Neo4jTransaction._
      import ExecutionContext.Implicits.global
      val node = process(p)
      val statement = node.toCypherCreateQuery.asCypherStatement
      import Neo4jREST._
      //println(Json.prettyPrint(Json.toJson(statement)))
      statement.apply()
      println(statement)

    } finally wsclient.close()
  }

  implicit class CypherNodeOps(n: Node) {
    def toCypherCreateQuery: Prepared = {
      import n._
      val (preparedNodes, preparedRelations) = relations.foldLeft((Prepared.empty, Prepared.empty)) {
        case ((nodes, relations), Relation(to, label, properties)) =>
          (nodes + to.toCypherCreateQuery, relations + c"CREATE ($id)-[:$label { $properties }]->(${to.id})")
      }
      c"CREATE ($id:$label { $properties })" + preparedNodes + preparedRelations
    }
  }

  def process: AstNode => Node = {

    case x @ Value(value) =>
      x.node("Value") + ("value" -> value)

    case x @ LiteralGroup(literal, value) => (
      x.node("LiteralGroup") + ("literal" -> literal)
        - prop("value") -> value
    )

    case x @ Argument(id, tpe, defaultValue) => (
      x.node("Argument")
        - prop("id")           -> id
        - prop("type")         -> tpe
        - prop("defaultValue") -> defaultValue
    )

    case x @ IdReference(to, typeApplication) => (
      x.node("IdReference")
        - prop("to")              -> to
        - prop("typeApplication") -< typeApplication
    )

    case x @ Package(path, body) =>
      val node = x.node("Package") - prop("body") -< body

      path map toNode[Id] match {
        case NonEmptySeq.FromSeq(x) => node - prop("path") -< x
        case _                      => node
      }

    case x @ Class(name, typeArguments, arguments, extensions, body) => (
      x.node("Class")
        - prop("typeArguments") -< typeArguments
        - prop("arguments")     -< arguments
        - prop("extensions")    -< extensions
        - prop("body")          -< body
        - prop("name")          -> name
    )

    case x @ Object(name, typeArguments, extensions, body) => (
      x.node("Object")
        - prop("typeArguments") -< typeArguments
        - prop("extensions")    -< extensions
        - prop("body")          -< body
        - prop("name")          -> name
    )

    case x @ Trait(name, typeArguments, arguments, extensions, body) => (
      x.node("Trait")
        - prop("typeArguments") -< typeArguments
        - prop("arguments")     -< arguments
        - prop("extensions")    -< extensions
        - prop("body")          -< body
        - prop("name")           -> name
    )

    case x @ Def(name, typeArguments, arguments, tpe, body) => (
      x.node("Def")
        - prop("typeArguments") -< typeArguments
        - prop("arguments")     -< arguments
        - prop("type")          -< tpe
        - prop("name")          -> name
        - prop("body")          -> body
    )

    case x @ Val(name, typeArguments, tpe, body) => (
      x.node("Val")
        - prop("typeArguments") -< typeArguments
        - prop("type")          -< tpe
        - prop("name")          -> name
        - prop("body")          -> body
    )

    case x @ TypeConstructor(name, typeArguments, body) => (
      x.node("TypeConstructor")
        - prop("typeArguments") -< typeArguments
        - prop("name")          -> name
        - prop("body")          -> body
    )

    case x @ UnimplementedMember(id, typeArguments, arguments, tpe) => (
      x.node("UnimplementedMember")
        - prop("id")            -> id
        - prop("typeArguments") -< typeArguments
        - prop("arguments")     -< arguments
        - prop("type")          -> tpe
    )

    case x @ MemberExtraction(target, names, source) =>
      val node = (
        x.node("MemberExtraction")
          - prop("source") -> source
          - prop("names")  -< names
      )
      toNode(target)
        .map(node - prop("target") -> _)
        .getOrElse(node)

    case x @ Marked(mark, statement) => (
      x.node("Marked")
        - prop("mark")      -> mark
        - prop("statement") -> statement
    )

    case x @ Comment(value) => (
      x.node("Comment")
        - prop("value") -> value
    )

    case x @ Import(value) => (
      x.node("Import")
        - prop("import") -> value
    )

    case x @ Import.Single(path) => (
      x.node("ImportSingle")
        - prop("path") -> path
    )

    case x @ Import.Multiple(path, parts) => (
      x.node("ImportMultiple")
        - prop("path")  -> path
        - prop("parts") -< parts
    )

    case x @ Import.Id(id) => (
      x.node("ImportId")
        - prop("id") -> id
    )

    case x @ Import.As(original, newId) => (
      x.node("ImportAs")
        - prop("original") -> original
        - prop("newId")    -> newId
    )

    case x @ Extension(id, expression) => (
      x.node("Extension")
        - prop("expression") -> expression
    )

    case x @ Application(target, argument) => (
      x.node("Application")
        - prop("target")   -> target
        - prop("argument") -> argument
    )

    case x @ Block(body) => (
      x.node("Block")
        - prop("body") -< body
    )

    case x @ BlockFunction(arguments, body) => (
      x.node("BlockFunction")
        - prop("arguments") -< arguments
        - prop("body")      -< body
    )

    case x @ ByName(expression) => (
      x.node("ByName")
        - prop("expression") -> expression
    )

    case x @ Function(arguments, body) => (
      x.node("Function")
        - prop("arguments") -< arguments
        - prop("body")      -> body
    )

    case x @ MarkedLiteralGroup(mark, literalGroup) => (
      x.node("MarkedLiteralGroup")
        - prop("mark")         -> mark
        - prop("literalGroup") -> literalGroup
    )

    case x @ MemberAccess(target, member) => (
      x.node("MemberAccess")
        - prop("target") -> target
        - prop("member") -> member
    )

    case x @ NamedProductApplication(target, arguments) => (
      x.node("NamedProductApplication")
        - prop("target")    -> target
        - prop("arguments") -< arguments
    )

    case x @ NamedExpression(name, expression) => (
      x.node("NamedExpression")
        - prop("name")       -> name
        - prop("expression") -> expression
    )

    case x @ Product(expressions) => (
      x.node("Product")
        - prop("expressions") -< expressions
    )

    case x @ ProductApplication(target, product) => (
      x.node("ProductApplication")
        - prop("target")  -> target
        - prop("product") -> product
    )

    case x @ Expression.Reference(to) => (
      x.node("Reference")
        - prop("to") -> to
    )

    case x @ WhitespaceApplication(target, method, argument) => (
      x.node("WhitespaceApplication")
        - prop("target")   -> target
        - prop("method")   -> method
        - prop("argument") -> argument
    )
  }

  implicit class AstNodeOps(x: AstNode) {
    def node(name: String) = Node(name, x.pos)

    def pos: Map[String, Any] = {
      val Position(start, end) = x.position
      Map("start" -> start, "end" -> end)
    }
  }

  case class Node(
    label: String,
    properties: Map[String, Any] = Map.empty,
    relations: Set[Relation] = Set.empty
  ) { self =>

    val id = newId

    case class - (r: Node => Relation) {
      def -> [A](o: A)(implicit ev: A => Node)  = self.copy(relations = self.relations + r(o))
      def -> [A](o: Option[A])(implicit ev: A => Node) =
        o match {
          case Some(o) => self.copy(relations = self.relations + r(o))
          case None    => self
        }

      def -< (o: NonEmptySeq[Node]): Node = self.copy(relations = self.relations + r(o.asChain(_ - "Next" -> _)))

      def -< (o: Seq[Node]): Node =
        o match {
          case NonEmptySeq.FromSeq(x) => this -< x
          case _ => self
        }
    }

    def + (prop: (String, Any)) = copy(properties = properties + prop)
  }

  case class Relation(to: Node, label: String, properties: Map[String, Any] = Map.empty)


  import language.implicitConversions
  implicit def stringToRelationBuilder(s: String): Node => Relation =
    to => Relation(to, s)


  implicit def _0(x: AstNode):Node = process(x)
  implicit def _1(x: AstNode | AstNode ): AstNode = x.merge
  implicit def _2[A](x: Seq[A])(implicit ev: A => AstNode): Seq[Node] = x.map(toNode[A])
  implicit def _3[A](x: Option[A])(implicit ev: A => AstNode): Seq[Node] = x.map(toNode[A]).toSeq
  implicit def _4[A](x: NonEmptySeq[A])(implicit ev: A => AstNode): NonEmptySeq[Node] = x.map(toNode[A])
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

  implicit class CypherHelper(val sc: StringContext) extends AnyVal {
    def c(args: Any*): Prepared = {
      val i = args.iterator
      val Seq(first, rest @ _*) = sc.parts
      val start = Prepared(first, Map.empty)
      rest.foldLeft(start) {
        case (Prepared(statement, arguments), part) =>
          i.next match {
            case id: String =>
              Prepared(s"$statement`$id`$part", arguments)
            case m: Map[String, Any] =>
              val id = newId
              Prepared(s"$statement`$id`$part", arguments + (id -> m))
          }
      }
    }
  }
}

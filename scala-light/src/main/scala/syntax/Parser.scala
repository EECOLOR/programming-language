package syntax

import fastparse.all._

/*
 * Statements vs Expressions
 * Declaration vs Invocation
 * Types vs Values
 */
object Parser {

  // shadow predef
  val any2stringadd = null

  import Separators._
  import ParserUtilities._
  import ast.Constructors._

  val parserEnhancements =
    new ParserEnhancements(
      whitespace        = ` \n`,
      spaces            = ` `,
      spacesSingleBreak = `  `
    )

  import parserEnhancements._

  val literalGroups = Seq("\"\"\"", "`", "\"", "'")
  val illegalInId   = "{}() \n.,:[]" + literalGroups.mkString
  val keywords      = Seq("=")
  val groupEscape   = "\\"

  object statements {

    import shared._
    import expressions._
    import ast.Expression
    import ast.Statement
    import ast.Statement._
    import syntax.UsefulDataTypes.|
    import syntax.UsefulDataTypes.NonEmptySeq
    import syntax.ast.Shared.Id

    val `package` = {
      PP( ("package" ~ ` ` commit (id.+ separatedBy ".") ~ `\n`).? ~ body ~ `\n` ~ End ).map(construct[Package])
    }

    val statement: P[Statement] =
      P( markedStatement.noCommit | unmarkedStatement | memberExtraction )

    val markedStatement =
      PP( id ~ ` ` ~ unmarkedStatement ).map(construct[Marked])

    val unmarkedStatement: P[Statement] =
      P(
        traitStatement   | objectStatement | classStatement |
        valStatement     | defStatement    | letStatement   |
        commentStatement | importStatement |
        unimplementedMemberStatement
      )

    val commentStatement =
      PP( "//" commit indexed( not("\n") ) ).map(construct[Comment])

    val importStatement: P[Import] = {
      import AlternativeParserBehavior.OrToEither

      val importSingle =
        PP( reference ).map(construct[Import.Single])

      val importMultiple = {
        val importId = PP( idReference ).map(construct[Import.Id])
        val importAs = PP( idReference.noCommit ` ` "=>" ` ` id ).map(construct[Import.As])
        val parts = P( commaSeparated("{" , (importAs | importId).+ , "}") ).map(construct[NonEmptySeq[Import.As | Import.Id]])

        PP( reference ~ "." ~ parts ).map(construct[Import.Multiple])
      }

      PP( "import" ~ ` ` commit (importMultiple | importSingle) ).map(construct[Import])
    }

    val traitStatement =
      PP( "trait" ~ ` ` commit id ~ typeArguments.? ~ valueArguments.? ~ extension.* ~(`  ` ~ block).? ).map(construct[Trait])

    val objectStatement =
      PP( "object" ~ ` ` commit id ~ typeArguments.? ~ extension.* ~ (`  ` ~ block).? ).map(construct[Object])

    val classStatement =
      PP( "class" ~ ` ` commit id ~ typeArguments.? ~ valueArguments ~ extension.* ~ (`  ` ~ block).? ).map(construct[Class])

    val extension = {
      val reference: P[Expression] = (product | (referenceExpression maybeFollowedBy (productApplication | namedProductApplication)))

      PP( `  ` ~ id ~ `  ` ~ reference ).map(construct[Extension])
    }

    val valStatement =
      PP( "val" ~ ` ` commit id ~ typeArguments.? ~ typeAscription.? ` ` "=" `  ` expression).map(construct[Val])

    val defStatement =
      PP( "def" ~ ` ` commit id ~ typeArguments.? ~ valueArguments.? ~ typeAscription.? ` ` "=" `  ` expression).map(construct[Def])

    val letStatement =
      PP( "type" ~ ` ` commit id ~ typeArguments.? ` ` "=" `  ` expression ).map(construct[TypeConstructor])

    val unimplementedMemberStatement =
      PP( (id ~ typeArguments.? ~ valueArguments.? ~ typeAscription).noCommit ~ &(`\n`) ).map(construct[UnimplementedMember])

    val memberExtraction = {
      val names = P( commaSeparated("(" , id.+ , ")") ).map(construct[NonEmptySeq[Id]])
      PP( reference.noCommit.? ~ ` `.? ~ names.noCommit ` ` "=" ~ `  ` commit  expression).map(construct[MemberExtraction])
    }

    val typeArguments =
      P( ` `.? ~ arguments("[", "]") )
  }

  object expressions {

    import shared._
    import ast.Expression
    import ast.Expression._

    val expression: P[Expression] =
      P( function | noWhitespaceExpression maybeFollowedBy whitespaceApplication)

    lazy val noWhitespaceExpression: P[Expression] =
      P( blockFunction | blockExpression | product | markedLiteralGroup | referenceExpression )
        .maybeFollowedBy(memberAccess, productApplication.noCommit, namedProductApplication, blockFunctionApplication, blockApplication)

    val function =
      PP( functionArguments ` ` "=>" ~ `  ` commit expression ).map(construct[Function])

    val blockFunction =
      PP("{" ` ` functionArguments ` ` "=>" ~ ` \n` commit body ~ ` \n`.? ~ "}").map(construct[BlockFunction])

    val blockExpression =
      PP( block ).map(construct[Block])

    val product =
      PP( commaSeparated("(" , expression.* , ")") ).map(construct[Product])

    val markedLiteralGroup =
      PP( literal.noCommit ~ literalGroup).map(construct[MarkedLiteralGroup])

    val referenceExpression =
      PP( reference ).map(construct[Reference])

    val whitespaceApplication =
      PP( ` ` ~ idReference ~ `  ` commit noWhitespaceExpression ).map(construct[Expression => WhitespaceApplication])

    val productApplication =
      PP( ` `.? ~ product )
        .map(construct[Expression => ProductApplication])

    val namedProductApplication =
      PP( ` `.? ~ commaSeparated("(" , (namedArgument.? ~ expression).* , ")") )
        .map(construct[Expression => NamedProductApplication])

    val blockFunctionApplication =
      PP(` `.? ~ blockFunction ).map(construct[Expression => Application])

    val blockApplication =
      PP( ` `.? ~ blockExpression ).map(construct[Expression => Application])

    val memberAccess =
      PP( `  `.? ~ "." commit idReference ).map(construct[Expression => MemberAccess])

    val functionArguments = {
      import AlternativeParserBehavior.OrToEither

      P(valueArguments.noCommit | id.noCommit)
    }

    val namedArgument =
      P( id.noCommit ` ` "=" ~ ` \n`.commit )
  }

  object shared {

    import ast.Shared._

    import statements.statement
    import expressions.expression

    val body = {
      import AlternativeParserBehavior.OrToEither

      P( (statement | expression).* separatedBy `\n` )
    }

    val idReference = {
      val typeApplication = commaSeparated( "[" , expression.+ , "]" )

      PP( id ~  typeApplication.? ).map(construct[IdReference])
    }

    val id = {
     import AlternativeParserBehavior.OrToEither

      P( literal | literalGroup )
    }

    val literal = {
      val legalInId = P( not(illegalInId) )
      val keyword = P( StringIn(keywords: _*) ~ !legalInId )

      P( indexed( !keyword ~ legalInId ) )
    }

    val literalGroup = {
      def toLiteralGroupParser(c: String) = {
        val escaped = (groupEscape ~ (c | groupEscape).!) | groupEscape.!
        val group   = (not(c + groupEscape) | escaped).*.map(_.mkString)

        PP(c.! commit indexed( group ) ~ c).map(construct[LiteralGroup])
      }

      P( literalGroups.map(toLiteralGroupParser).reduce(_ | _) )
    }

    def indexed(p: => Parser[String]) =
      PP(p).map(construct[Value])

    val reference =
      P( idReference.+ separatedBy "." ).map(construct[Reference])

    val block =
      group("{" , body.? , "}")

    val valueArguments =
      P( ` `.? ~ arguments("(", ")") )

    def arguments(`<`: String, `>`: String) = {
      val defaultValue = P(` ` ~ "=" ~ `  ` commit expression)
      val argument = PP( id ~ typeAscription.? ~ defaultValue.? ).map(construct[Argument])

      commaSeparated(`<` , argument.* , `>`)
    }

    def commaSeparated[A](`<`: String, body: Parser[A] with Separator[A], `>`: String) =
      group(`<` , body separatedBy `,` , `>`)

    def group[A](`<`: String, body: Parser[A], `>`: String) =
      P( `<` commit ` \n`.? ~ body ~ ` \n`.? ~ `>` )

    val typeAscription =
      P( ` `.? ~ ":" ~ ` ` commit expression )

    def PP[T](parser: => Parser[T]) =
      P(Index ~ parser ~ Index).map { case (start, value, end) => (value, ast.Position(start, end)) }
  }
}

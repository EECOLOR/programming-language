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
    import ast.Statement
    import ast.Statement._

    val `package` = {
      P( ("package" ~ ` ` commit (id.+ separatedBy ".") ~ `\n`).? ~ body ~ `\n` ~ End ).map(construct[Package])
    }

    val statement: P[Statement] =
      P( markedStatement.noCommit | unmarkedStatement )

    val markedStatement =
      P( id ~ ` ` ~ unmarkedStatement ).map(Marked)

    val unmarkedStatement: P[Statement] =
      P(
        traitStatement   | objectStatement | classStatement |
        valStatement     | defStatement    | letStatement   |
        commentStatement | importStatement |
        memberExtraction | unimplementedMemberStatement
      )

    val commentStatement =
      P( "//" commit indexed( not("\n") ) ).map(Comment)

    val importStatement: P[Import] = {

      val importSingle =
        P( reference ).map(Import.Single)

      val importMultiple = {
        val importId = P( id ).map(Import.Id)
        val importAs = P( id.noCommit ` ` "=>" ` ` id ).map(Import.As)

        P( reference ~ "." ~ commaSeparated("{" , (importAs | importId).+ , "}") ).map(Import.Multiple)
      }

      P( "import" ~ ` ` commit (importMultiple | importSingle) )
    }

    val traitStatement =
      P( "trait" ~ ` ` commit id ~ typeArguments.? ~ valueArguments.? ~ extension.* ~(`  ` ~ block).? ).map(construct[Trait])

    val objectStatement =
      P( "object" ~ ` ` commit id ~ typeArguments.? ~ extension.* ~ (`  ` ~ block).? ).map(construct[Object])

    val classStatement =
      P( "class" ~ ` ` commit id ~ typeArguments.? ~ valueArguments ~ extension.* ~ (`  ` ~ block).? ).map(construct[Class])

    val extension =
      P( `  ` ~ id ~ `  ` ~ (product | (referenceExpression maybeFollowedBy (productApplication| namedProductApplication))) ).map(Extension)

    val valStatement =
      P( "val" ~ ` ` commit id ~ typeArguments.? ~ typeAscription.? ` ` "=" `  ` expression).map(construct[Val])

    val defStatement =
      P( "def" ~ ` ` commit id ~ typeArguments.? ~ valueArguments.? ~ typeAscription.? ` ` "=" `  ` expression).map(construct[Def])

    val letStatement =
      P( "let" ~ ` ` commit id ~ typeArguments.? ` ` "=" `  ` expression ).map(construct[TypeConstructor])

    val unimplementedMemberStatement =
      P( (id ~ typeArguments.? ~ valueArguments.? ~ typeAscription).noCommit ~ &(`\n`) ).map(construct[UnimplementedMember])

    val memberExtraction =
      P( reference.noCommit.? ~ ` `.? ~ commaSeparated("(" , id.+ , ")").noCommit ` ` "=" ~ `  ` commit  expression).map(MemberExtraction)

    val typeArguments =
      P( ` `.? ~ arguments("[", "]") )
  }

  object expressions {

    import shared._
    import ast.Expression
    import ast.Expression._

    val expression: P[Expression] =
      P( function | blockFunction | noFunctionExpression )

    val noFunctionExpression =
      P( noWhitespaceExpression maybeFollowedBy whitespaceApplication)

    lazy val noWhitespaceExpression: P[Expression] =
      P( blockExpression | product | markedLiteralGroup | referenceExpression )
        .maybeFollowedBy(memberAccess, productApplication, blockFunctionApplication, blockApplication, namedProductApplication)

    val function =
      P( functionArguments ` ` "=>" ~ `  ` commit expression ).map(construct[Function])

    val blockFunction =
      P("{" ` ` functionArguments ` ` "=>" ~ ` \n` commit body ~ ` \n`.? ~ "}").map(construct[BlockFunction])

    val blockExpression =
      P( block ).map(construct[Block])

    val product =
      P( commaSeparated("(" , expression.* , ")") ).map(Product)

    val markedLiteralGroup =
      P( literal.noCommit ~ literalGroup).map(MarkedLiteralGroup)

    val referenceExpression =
      P( reference ).map(Reference)

    val whitespaceApplication =
      P( ` ` ~ idReference ~ `  ` commit noWhitespaceExpression ).map(construct[Expression => WhitespaceApplication])

    val productApplication =
      P( ` `.? ~ commaSeparated("(" , expression.* , ")") )
        .map(construct[Expression => ProductApplication])

    val namedProductApplication =
      P( ` `.? ~ commaSeparated("(" , (namedArgument.? ~ expression).* , ")") )
        .map(construct[Expression => NamedProductApplication])

    val blockFunctionApplication =
      P(` `.? ~ blockFunction ).map(construct[Expression => Application])

    val blockApplication =
      P( ` `.? ~ blockExpression ).map(construct[Expression => Application])

    val memberAccess =
      P( `  `.? ~ "." commit idReference ).map(construct[Expression => MemberAccess])

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
    import expressions.noFunctionExpression

    val body = {
      import AlternativeParserBehavior.OrToEither

      P( (statement | expression).* separatedBy `\n` )
    }

    val idReference = {
      val typeApplication = commaSeparated( "[" , expression.+ , "]" )

      P( id ~  typeApplication.? ).map(construct[IdReference])
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

        (c commit indexed( group ) ~ c).map(LiteralGroup(c, _))
      }

      P( literalGroups.map(toLiteralGroupParser).reduce(_ | _) )
    }

    def indexed(p: => Parser[String]) =
      (Index ~ p).map(Indexed)

    val reference =
      P( idReference.+ separatedBy "." )

    val block =
      group("{" , body.? , "}")

    val valueArguments =
      P( ` `.? ~ arguments("(", ")") )

    def arguments(`<`: String, `>`: String) = {
      val defaultValue = P(` ` ~ "=" ~ `  ` commit expression)
      val argument = P( id ~ typeAscription.? ~ defaultValue.? ).map(Argument)

      commaSeparated(`<` , argument.* , `>`)
    }

    def commaSeparated[A](`<`: String, body: Parser[Seq[A]] with Separator[A], `>`: String) =
      group(`<` , body separatedBy `,` , `>`)

    def group[A](`<`: String, body: Parser[A], `>`: String) =
      P( `<` commit ` \n`.? ~ body ~ ` \n`.? ~ `>` )

    val typeAscription =
      P( ` `.? ~ ":" ~ ` ` commit noFunctionExpression )
  }
}

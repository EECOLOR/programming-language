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
  import ast._

  val parserEnhancements =
    new ParserEnhancements(
      whitespace        = ` \n`,
      spaces            = ` `,
      spacesSingleBreak = `  `
    )

  import parserEnhancements._

  val literalGroups = Seq("\"\"\"", "`", "\"", "'")
  val illegalInId   = "{}() \n.,:[]" + literalGroups.mkString
  val keywords      = Seq("package", "import", "object", "trait", "class", "val", "def", "let", "=")
  val groupEscape   = "\\"

  object cores {
    import Core._

    val body = {
        import statements.statement
        import expressions.expression
        import AlternativeParserBehavior.OrToEither

        P( (statement | expression).* separatedBy `\n` ).map(Body)
      }

    val extension = {
      import expressions.productExpression
      import expressions.referenceExpression
      import expressions.productApplicationExpression

      P( `  ` ~ id ~ `  ` ~ (productExpression | (referenceExpression maybeFollowedBy productApplicationExpression)).? ).map(Extension)
    }

    val typeApplication = {
      import expressions.expression

      P( "[" ~/ ` \n`.? ~ (expression.+ separatedBy `,`) ~ ` \n`.? ~ "]" ).map(TypeApplication)
    }

    val reference =
      P( id ~  typeApplication.? ).map(Reference)

    val qualifiedReference =
      P( reference.+ separatedBy "." ).map(QualifiedReference)

    val block =
      P( "{" ~/ (` \n` ~ body).? ~ ` \n`.? ~ "}" ).map(Block)

    val valueArguments =
      arguments("(", ")")

    val typeArguments =
      arguments("[", "]")

    def arguments(`<`: String, `>`: String) =
      P( `<` ~/ ` \n`.? ~ (argument.* separatedBy `,`) ~ ` \n`.? ~ `>` ).map(Arguments)

    val argument = {
      import expressions.expression

      P( optionalTyped(id) ~ (` ` ~ "=" ~ `  ` ~/ expression).? ).map(Argument)
    }

    val id = {
     import AlternativeParserBehavior.OrToEither


      P( literal | literalGroup ).map(Id)
    }

    val literal = {
      val legalInId = P( not(illegalInId) )
      val keyword = P( StringIn(keywords: _*) ~ !legalInId )

      P( indexed( !keyword ~ legalInId ) )
    }

    val qualifiedId =
      P( id.+ separatedBy "." ).map(QualifiedId)

    val literalGroup = {
      def toLiteralGroupParser(c: String) = {
        val escaped = (groupEscape ~ (c | groupEscape).!) | groupEscape.!
        val group   = (not(c + groupEscape) | escaped).*.map(_.mkString)

        (c ~/ indexed( group ) ~ c).map(Core.LiteralGroup(c, _))
      }

      P( literalGroups.map(toLiteralGroupParser).reduce(_ | _) )
    }

    def optionalTyped[A](p: Parser[A]) =
      P( p ~ typeAscription.? ).map(Typed[A])

    def typed[A](p: Parser[A]) =
      P( p ~ typeAscription.map(Option(_)) ).map(Typed[A])

    val typeAscription = {
      import expressions.noFunctionExpression

      P( ` `.? ~ ":" ~ ` ` ~/ noFunctionExpression )
    }

    def indexed(p: => Parser[String]) =
      (Index ~ p).map(Indexed)
  }

  object statements {

    import Statement._

    val `package` = {
      import cores.body
      import cores.qualifiedId

      P( ("package" ~ " " ~/ qualifiedId ~ `\n`).? ~ body ~ `\n` ~ End ).map(Package)
    }

    val statement: P[Statement] =
      P( markedStatement | unmarkedStatement )

    val markedStatement = {
      import cores.id

      P( NoCut(id) ~ ` ` ~ unmarkedStatement ).map(Marked)
    }

    val unmarkedStatement: P[Statement] =
      P(
        traitStatement   | objectStatement | classStatement |
        valStatement     | defStatement    | letStatement   |
        commentStatement | importStatement |
        memberExtraction | unimplementedMemberStatement
      )

    val commentStatement = {
      import cores.indexed

      P( "//" ~/ indexed( not("\n") ) ).map(Comment)
    }

    val importStatement: P[Import] =
      P( "import" ~ " " ~/ (importMultiple | importSingle) )

    val importSingle = {
      import cores.qualifiedReference
      P( qualifiedReference ).map(Import.Single)
    }

    val importMultiple = {
      import cores.qualifiedReference
      P( qualifiedReference ~ ".{" ~/ ` \n` ~ ((importAs | importId).+ separatedBy `,`) ` \n` "}" ).map(Import.Multiple)
    }

    val importId = {
      import cores.id

      P( id ).map(Import.Id)
    }

    val importAs = {
      import cores.id

      P( id ` ` "=>" ` ` id ).map(Import.As)
    }

    val traitStatement = {
      import cores.typeArguments
      import cores.valueArguments
      import cores.block
      import cores.extension
      import cores.id

      P( "trait" ~/ ` ` ~ id ~ (` `.? ~ typeArguments).? ~ (` `.? ~ valueArguments).? ~ extension.* ~(` ` ~ block).? ).map(Statement.Trait)
    }

    val objectStatement = {
      import cores.typeArguments
      import cores.block
      import cores.extension
      import cores.id

      P( "object" ~ ` ` ~/ id ~ (` `.? ~ typeArguments).? ~ extension.* ~ (` ` ~ block).? ).map(Statement.Object)
    }

    val classStatement = {
      import cores.typeArguments
      import cores.valueArguments
      import cores.block
      import cores.extension
      import cores.id

      P( "class" ~ ` ` ~/ id ~ (` `.? ~ typeArguments).? ~ ` `.? ~ valueArguments ~ extension.* ~ (` ` ~ block).? ).map(Statement.Class)
    }

    val valStatement = {
      import cores.typeArguments
      import expressions.expression
      import cores.id
      import cores.optionalTyped

      P( "val" ~ ` ` ~/ optionalTyped(id ~ (` `.? ~ typeArguments).?) ` ` "=" ` \n` expression).map(Statement.Val)
    }

    val defStatement = {
      import cores.typeArguments
      import cores.valueArguments
      import expressions.expression
      import cores.id
      import cores.optionalTyped

      P( "def" ~ ` ` ~/ optionalTyped(id ~/ (` `.? ~ typeArguments).? ~ (` `.? ~ valueArguments).?) ` ` "=" ~ `  ` ~ expression).map(Statement.Def)
    }

    val letStatement = {
      import cores.typeArguments
      import expressions.expression
      import cores.id

      P( "let" ~ ` ` ~/ id ~ typeArguments ` ` "=" ` \n` expression ).map(Statement.TypeConstructor)
    }

    val unimplementedMemberStatement = {
      import cores.typeArguments
      import cores.valueArguments
      import cores.id
      import cores.typed

      P( typed(NoCut(id ~ (` `.? ~ typeArguments).? ~ (` `.? ~ valueArguments).?)) ).map(Statement.UnimplementedMember)
    }

    val memberExtraction = {
      import cores.qualifiedReference
      import expressions.expression
      import cores.id

      P( NoCut(qualifiedReference.?) ~ ` `.? ~ "(" ~ `  `.? ~ NoCut(id.+ separatedBy `,`) ~ `  `.? ~ ")" ` ` "=" ~ `  ` ~/  expression).map(Statement.MemberExtraction)
    }
  }

  object expressions {

    import Expression._

    val expression: P[Expression] =
      P( functionExpression | noFunctionExpression )

    val functionExpression = {
      import cores.valueArguments
      import cores.id
      import AlternativeParserBehavior.OrToEither
      import cores.optionalTyped

      P( (NoCut(valueArguments) | NoCut(optionalTyped(id))) ` ` "=>" ~ `  ` ~/ expression ).map(Function)
    }

    val noFunctionExpression: P[Expression] =
      P( noWhitespaceApplicationExpression )
        .maybeFollowedBy(whitespaceApplicationExpression)

    def whitespaceApplicationExpression(e: Expression) = {
      import cores.reference

      P( ` ` ~ reference ~ `  ` ~/ noWhitespaceApplicationExpression ).map(WhitespaceApplication(e, _))
    }

    val noWhitespaceApplicationExpression: P[Expression] =
      P( blockFunctionExpression | blockExpression | markedLiteralGroup | referenceExpression | productExpression )
        .maybeFollowedBy(
          memberAccessExpression,
          productApplicationExpression,
          blockFunctionApplicationExpression,
          blockApplicationExpression
        )

    val blockFunctionExpression = {
      import cores.valueArguments
      import cores.body
      import AlternativeParserBehavior.OrToEither
      import cores.id
      import cores.optionalTyped

      P("{" ` ` (NoCut(valueArguments) | NoCut(optionalTyped(id))) ` ` "=>" ~ ` \n` ~/ body ~ ` \n`.? ~ "}").map(BlockFunction)
    }

    val blockExpression = {
      import cores.block

      P( block ).map(Block)
    }

    val referenceExpression = {
      import cores.qualifiedReference

      P( qualifiedReference ).map(Reference)
    }

    val productExpression = {
      import cores.id

      P( "(" ~/ ` \n`.? ~ (((NoCut(id) ` ` "=" ~ ` \n`.~/).? ~ expression).* separatedBy `,`) ~ ` \n`.? ~ ")" ).map(Product)
    }

    val markedLiteralGroup = {
      import cores.literal
      import cores.literalGroup

      P( NoCut(literal) ~ literalGroup).map(MarkedLiteralGroup)
    }

    def productApplicationExpression(e: Expression) =
      P( ` `.? ~ productExpression ).map(ProductApplication(e, _))

    def blockFunctionApplicationExpression(e: Expression) =
      P(` `.? ~ blockFunctionExpression ).map(BlockFunctionApplication(e, _))

    def blockApplicationExpression(e: Expression) =
      P( ` `.? ~ blockExpression ).map(Expression.BlockApplication(e, _))

    def memberAccessExpression(e: Expression) = {
      import cores.qualifiedReference

      P( `  `.? ~ "." ~/ qualifiedReference ).map(MemberAccess(e, _))
    }
  }
}

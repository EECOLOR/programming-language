package syntax

import fastparse.all._

object Parser {

  import Whitespace._
  import ParserUtilities._
  import ast._

  val parserEnhancements =
    new ParserEnhancements(
      whitespace        = ` \n`,
      spaces            = ` `,
      spacesSingleBreak = `  `
    )

  import parserEnhancements._

  val literalGroups = Seq("\"\"", "`", "\"", "'")
  val illegalInId   = "{}() \n.,:[]" + literalGroups.mkString
  val keywords      = Seq("package", "import", "object", "trait", "class", "val", "def", "//", "=")
  val groupEscape   = "\\"

  val scalalight =
    P( ("package" ~ " " ~/ qualifiedId ~ ` \n`).? ~ body ~ `\n` ~ End ).map(Core.Package)

  val body: P[Core.Body] = {
    import AlternativeParserBehavior.OrToEither

    P( (statement | expression).separatedBy(`\n`) ).map(Core.Body)
  }

    val statement: P[Statement] =
      P( markedStatement | unmarkedStatement )

      val markedStatement =
        P( NoCut(id) ~ ` ` ~ unmarkedStatement ).map(Statement.Marked)

      val unmarkedStatement =
        P(
          commentStatement |
          importStatement  |
          traitStatement   | objectStatement | classStatement |
          valStatement     | defStatement    |
          unimplementedMemberStatement
        )

          val commentStatement =
            P( "//" ~/ indexed( not("\n") ) ).map(Statement.Comment)

          val importStatement =
            P( "import" ~ " " ~/ (importMultiple | importSingle) )

            val importSingle =
              P( qualifiedReference ).map(Statement.Import.Single)

            val importMultiple =
              P( qualifiedReference ~ ".{" ~/ ` \n` ~ (importAs | importId).rep(min = 1, commaSeparator) ` \n` "}" ).map(Statement.Import.Multiple)

              val importId =
                P( id ).map(Statement.Import.Part.Id)

              val importAs =
                P( id ` ` "=>" ` ` id ).map(Statement.Import.Part.As)

          val traitStatement =
            P( "trait" ~/ ` ` ~ id ~ (` `.? ~ typeArguments).? ~ (` `.? ~ arguments).? ~ (` ` ~ block).? ).map(Statement.Trait)

          val objectStatement =
            P( "object" ~ ` ` ~/ id ~ (` ` ~ block).? ).map(Statement.Object)

          val classStatement =
            P( "class" ~ ` ` ~/ id ~ (` `.? ~ typeArguments).? ~ ` `.? ~ arguments ).map(Statement.Class)

          val valStatement =
            P( "val" ~ ` ` ~/ optionalTyped(id ~ (` `.? ~ typeArguments).?) ` ` "=" ` \n` expression).map(Statement.Val)

          val defStatement =
            P( "def" ~ ` ` ~/ optionalTyped(id ~/ (` `.? ~ typeArguments).? ~ (` `.? ~ arguments).?) ` ` "=" ~ `  ` ~ expression).map(Statement.Def)

          val unimplementedMemberStatement =
            P( typed(NoCut(id ~ (` `.? ~ typeArguments).? ~ (` `.? ~ arguments).?)) ).map(Statement.UnimplementedMember)

    val expression: P[Expression] =
      P( functionExpression | noFunctionExpression )

      val functionExpression = {
        import AlternativeParserBehavior.OrToEither

        P( (NoCut(arguments) | NoCut(optionalTyped(id)) | NoCut(productExpression)) ` ` "=>" ~ `  ` ~/ expression ).map(Expression.Function)
      }

      val noFunctionExpression: P[Expression] =
        P( noWhitespaceApplicationExpression )
          .maybeFollowedBy(whitespaceApplicationExpression)

        def whitespaceApplicationExpression(e: Expression) =
          P( ` ` ~ reference ~ `  ` ~/ noWhitespaceApplicationExpression ).map(Expression.WhitespaceApplication(e, _))

        val noWhitespaceApplicationExpression: P[Expression] =
          P( blockFunctionExpression | blockExpression | markedLiteralGroup | referenceExpression | productExpression )
            .maybeFollowedBy(
              memberAccessExpression,
              productApplicationExpression,
              blockFunctionApplicationExpression,
              blockApplicationExpression
            )

          val blockFunctionExpression = {
            import AlternativeParserBehavior.OrToEither

            P("{" ` ` (NoCut(arguments) | NoCut(optionalTyped(id))) ` ` "=>" ~ ` \n` ~/ body ~ ` \n` ~ "}").map(Expression.BlockFunction)
          }

          val blockExpression =
            P( block ).map(Expression.Block)

          val referenceExpression =
            P( qualifiedReference ).map(Expression.Reference)

          val productExpression =
            P( "(" ~/ ` \n`.? ~ ((NoCut(id) ` ` "=" ~ ` \n`.~/).? ~ expression).rep(sep = commaSeparator.~/) ~ ` \n`.? ~ ")" ).map(Expression.Product)

          val markedLiteralGroup =
            P( NoCut(literal) ~ literalGroup).map(Expression.MarkedLiteralGroup)

          def productApplicationExpression(e: Expression) =
            P( ` `.? ~ productExpression ).map(Expression.ProductApplication(e, _))

          def blockFunctionApplicationExpression(e: Expression) =
            P(` `.? ~ blockFunctionExpression ).map(Expression.BlockFunctionApplication(e, _))

          def blockApplicationExpression(e: Expression) =
            P( ` `.? ~ blockExpression ).map(Expression.BlockApplication(e, _))

          def memberAccessExpression(e: Expression) =
            P( `  `.? ~ "." ~/ qualifiedReference ).map(Expression.MemberAccess(e, _))

  val keyword =
    P( StringIn(keywords: _*) ~ !legalInId )

  val legalInId =
    P( not(illegalInId) )

  val id = {
   import AlternativeParserBehavior.OrToEither


    P( literal | literalGroup ).map(Core.Id)
  }

  val literal =
    P( indexed( !keyword ~ legalInId ) )

  val qualifiedId =
    P( id.rep(min = 1, sep = "." ) ).map(Core.QualifiedId)

  val reference: P[Core.Reference] = {
    P( id ~  typeApplication.? ).map(Core.Reference)
  }

  val literalGroup =
    P( literalGroups.map(toLiteralGroupParser).reduce(_ | _) )

  def toLiteralGroupParser(c: String) = {
    val escaped = (groupEscape ~ (c | groupEscape).!) | groupEscape.!
    val group   = (not(c + groupEscape) | escaped).rep.map(_.mkString)

    (c ~/ indexed( group ) ~ c).map(Core.LiteralGroup(c, _))
  }

  val typeApplication =
    P( "[" ~/ ` \n`.? ~ qualifiedReference.rep(min = 1, commaSeparator) ~ ` \n`.? ~ "]" ).map(Core.TypeApplication)

  val qualifiedReference =
    P( reference.rep(min = 1, sep = "." ) ).map(Core.QualifiedReference)

  val block =
    P( "{" ~/ (` \n` ~ body ~ ` \n`).? ~ "}" ).map(Core.Block)

  val arguments =
    P( "(" ~/ ` \n`.? ~ argument.rep(sep = commaSeparator) ~ ` \n`.? ~ ")" ).map(Core.Arguments)

  val argument =
    P( optionalTyped(id) ~ (` ` ~ "=" ~ `  ` ~/ expression).? ).map(Core.Argument)

  val commaSeparator =
    P( ` \n`.? ~ "," ~/ ` \n` )

  val typeAscription =
    P( ` `.? ~ ":" ~/ ` ` ~ expression )

  val typeArguments =
    P( "[" ~/ ` \n`.? ~ argument.rep(sep = commaSeparator) ~ ` \n`.? ~ "]" ).map(Core.Arguments)

  def optionalTyped[A](p: Parser[A]) =
    P( p ~ typeAscription.? ).map(Core.Typed[A])

  def typed[A](p: Parser[A]) =
    P( p ~ typeAscription.map(Option(_)) ).map(Core.Typed[A])

  def indexed(p: => Parser[String]) =
    (Index ~ p).map(Core.Indexed)
}

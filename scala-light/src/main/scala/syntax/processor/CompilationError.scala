package syntax.processor

import syntax.ast.AstNode

case class CompilationError(message: String, ast: AstNode)

object CompilationError {

   def NoExtensionsError(x: AstNode) =
    CompilationError("Extensions are not supported yet", x)

  def NoDefaultValuesError(x: AstNode) =
    CompilationError("Default values are not supported yet", x)

  def NoUnimplementedMembersInObjectError(x: AstNode) =
    CompilationError("Objects can not have unimplemented members", x)

  def ArgumentWithoutTypeAsUnimplmentedMemberError(x: AstNode) =
    CompilationError("Argument, intended as member, should have a type", x)

  def UnexpectedUnimplementedMember(x: AstNode) =
    CompilationError("Did not expect and unimplemented member in this position", x)

  def UnexpectedExpressionInStatementPosition(x: AstNode) =
    CompilationError("Did not expect an expression at this position, expected statement", x)

  def TypeArgumentsNotSupportedError(x: AstNode) =
    CompilationError("Type arguments on unimplemented members are not supported yet", x)

  def ArgumentsNotSupportedError(x: AstNode) =
    CompilationError("Arguments on unimplemented members are not supported yet", x)

  def MemberExtractionNotSupportedError(x: AstNode) =
    CompilationError("Member extraction not supported yet", x)
}

package syntax

import syntax.ast.AstNode
import syntax.processor.ProcessedAstNode

case class CompilationError(message: String, ast: AstNode) extends ProcessedAstNode

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
}

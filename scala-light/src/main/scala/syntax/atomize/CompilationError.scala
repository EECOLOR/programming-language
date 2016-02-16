package syntax.atomize

import syntax.processor.ProcessedAstNode
import syntax.processor.Shared.{
  Id => AstId
}

case class CompilationError(message: String, node: ProcessedAstNode)

object CompilationError {
  def UnsupportedMarkedStatement(mark: AstId) =
    CompilationError("Unspoorted mark statement: " + mark.fold(_.value, x => x.literal + x.value + x.literal), mark.fold(x => x, x => x))
}

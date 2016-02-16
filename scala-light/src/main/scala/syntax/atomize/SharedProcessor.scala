package syntax.atomize

import syntax.Processor
import syntax.processor.Shared.{
  Argument => AstArgument,
  Value => AstValue,
  LiteralGroup => AstLiteralGroup
}

object SharedProcessor {

  import Atom._
  import ExpressionProcessor.{ processor => expressionProcessor }
  import Processor._
  import Shared._
  import Shared.context._

  implicit val processArgument:
    ArgumentContainer -> Argument = P {
      case ArgumentContainer(AstArgument(name, tpe), defaultType) =>
      for {
        newType <- process(tpe)
        newName <- process(name)
      } yield NameTypePair(newName, newType getOrElse defaultType)
    }

  implicit val processValue: AstValue -> Value = P {
    case x @ AstValue(value) => Result(Value(value)(x))
  }

  implicit val processLiteralGroup: AstLiteralGroup -> LiteralGroup = P {
    case x @ AstLiteralGroup(literal, value) => Result(LiteralGroup(literal, value)(x))
  }
}

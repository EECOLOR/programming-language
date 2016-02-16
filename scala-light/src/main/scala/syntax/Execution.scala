//package syntax
//
//import syntax.ast.Shared
//import syntax.ast.Shared._
//import syntax.ast.Statement
//import syntax.ast.Statement._
//import syntax.ast.Expression
//
//object Execution {
//  def execute(body: Seq[Statement | Expression]): Unit = {
//    val vals = body.collect { case Left(Val(Left(id), _, _, body)) => id.value -> body }.toMap
//    val builtIn = Map[String, Function](
//      "println" -> Function {
//        case StringValue(value) => {
//          println(value)
//          SideEffect
//        }
//      }
//    )
//
//    def resolve(expression: Expression): Resolved = {
//      expression match {
//        case Expression.Reference(Seq(Shared.IdReference(Left(id), _))) =>
//          if (vals contains id.value) ValReference(id.value)
//          else UnknownReference(id.value)
//        case Expression.Reference(Seq(Shared.IdReference(id, _))) =>
//          resolveReference(id)
//      }
//    }
//
//    val resolvedVals = vals.mapValues(resolve)
//
//    val applications = body.collect { case Right(Expression.ProductApplication(target, Seq(argument))) =>
//      resolve(target) match {
//        case UnknownReference(to) =>
//          Application(builtIn(to), resolve(argument))
//      }
//    }
//
//    applications.foreach {
//      case Application(f, a) =>
//        println(f.f(a))
//    }
//  }
//
//  def resolveReference(to: Id) = {
//    to match {
//      case Right(LiteralGroup("\"", indexed)) => StringValue(indexed.value)
//      case Left(id) => UnknownReference(id.value)
//    }
//  }
//
//  trait Resolved
//  case class ValReference(to: String) extends Resolved
//  case class StringValue(value: String) extends Resolved
//  case class Application(target: Function, argument: Resolved) extends Resolved
//  case class Function(f: Resolved => Resolved) extends Resolved
//  case class UnknownReference(to: String) extends Resolved
//  case object SideEffect extends Resolved
//
//  def asString(id: Id) = id.fold(_.value, x => x.literal + x.value + x.literal)
//}

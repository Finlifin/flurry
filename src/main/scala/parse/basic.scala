package parse

import scala.util.control.Breaks._
import scala.util.control.Breaks
import scala.util.boundary

type ParseResult = Either[ParseError, Option[Ast]]

def result(None: Option[Ast]): ParseResult = Right(None)
def result(node: Ast): ParseResult = Right(Some(node))
def result(error: ParseError): ParseResult = Left(error)

// 抽象函数：处理Parser的enter和exit调用
inline def withCtx[T](parser: Parser, prefix: Option[lex.Tag] = None, dontEat: Boolean = false)(
    block: => ParseResult
): ParseResult = boundary {
  parser.enter()
  try {
    prefix match {
      case Some(tag) =>
        if (!dontEat) { if (!parser.eatToken(tag)) { boundary.break(result(None)) } }
        else { if (!parser.peek(tag)) { boundary.break(result(None)) } }
      case None => ()
    }

    block
  } finally parser.exit()
}

// TODO: escaping rules
def tryAtom(parser: Parser): ParseResult = withCtx(parser) {
  val token = parser.peekToken()
  token.tag match
    case lex.Tag.int => result(Ast.Integer(parser.srcContentT(parser.nextToken()).toInt).withSpan(parser.currentSpan()))
    case lex.Tag.real => result(Ast.Real(parser.srcContentT(parser.nextToken()).toFloat).withSpan(parser.currentSpan()))
    case lex.Tag.str => result(Ast.Str(parser.srcContentT(parser.nextToken())).withSpan(parser.currentSpan()))
    case lex.Tag.id => result(Ast.Id(parser.srcContentT(parser.nextToken())).withSpan(parser.currentSpan()))
    case lex.Tag.char =>
      result(Ast.LitChar(parser.srcContentT(parser.nextToken()).charAt(1)).withSpan(parser.currentSpan()))
    case lex.Tag.k_true =>
      parser.eatTokens(1)
      result(Ast.Bool(true).withSpan(parser.currentSpan()))
    case lex.Tag.k_false =>
      parser.eatTokens(1)
      result(Ast.Bool(false).withSpan(parser.currentSpan()))
    case lex.Tag.k_null =>
      parser.eatTokens(1)
      result(Ast.NullVal.withSpan(parser.currentSpan()))
    case lex.Tag.k_self =>
      parser.eatTokens(1)
      result(Ast.SelfVal.withSpan(parser.currentSpan()))
    case lex.Tag.k_itself =>
      parser.eatTokens(1)
      result(Ast.Itself.withSpan(parser.currentSpan()))
    case lex.Tag.k_Self =>
      parser.eatTokens(1)
      result(Ast.SelfType.withSpan(parser.currentSpan()))
    case _ => result(None)
}

// 我想淘汰，还没重构完
def tryId(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.id), true) {
  result(Ast.Id(parser.srcContentT(parser.nextToken())).withSpan(parser.currentSpan()))
}

def trySymbol(parser: Parser): ParseResult = withCtx(parser) {
  if (parser.peek(lex.Tag.`.`, lex.Tag.id)) {
    parser.eatTokens(1)
    result(Ast.Symbol(parser.srcContentT(parser.nextToken())).withSpan(parser.currentSpan()))
  } else result(None)
}

def tryProperty(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.`.`, lex.Tag.id)) { boundary.break(result(None)) }

    parser.eatTokens(1)
    val id = parser.srcContentT(parser.nextToken())

    val value = tryExpr(parser) match
      case Right(Some(expr)) => expr
      case Left(err) => boundary.break(result(err))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after property name")))

    result(Ast.Property(id, value).withSpan(parser.currentSpan()))
  }
}

def tryPropertyAssign(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.`.`, lex.Tag.id, lex.Tag.`=`)) { boundary.break(result(None)) }

    parser.eatTokens(1)
    val id = parser.srcContentT(parser.nextToken())

    if (!parser.eatToken(lex.Tag.`=`)) boundary
      .break(result(parser.invalidTerm("=", "expected '=' after property name")))

    val value = tryExpr(parser) match
      case Right(Some(expr)) => expr
      case Left(err) => boundary.break(result(err))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after '='")))

    result(Ast.PropertyAssign(id, value).withSpan(parser.currentSpan()))
  }
}

// // ^expr term
// def tryAttribute(parser: Parser, followRule: Parser => ParseResult): ParseResult = withCtx(parser, Some(lex.Tag.`^`)) {
//   boundary {
//     val attr = tryExpr(parser, ExprOption(precedence = 90)) match
//       case Right(Some(node)) => node
//       case Left(e) => boundary.break(result(e))
//       case _ => boundary.break(result(None))

//     val term = followRule(parser) match
//       case Right(Some(node)) => node
//       case Left(e) => boundary.break(result(e))
//       case _ => boundary.break(result(None))

//     result(AstNode2(Tag.attribute, parser.currentSpan(), attr, term))
//   }
// }

// def tryPrefixTerm(parser: Parser, tag: Tag, prefixToken: lex.Tag, followRule: Parser => ParseResult): ParseResult =
//   withCtx(parser, Some(prefixToken)) {
//     followRule(parser) match
//       case Right(Some(node)) => result(Ast.Inline)
//       case Left(e) => result(e)
//       case _ =>
//         parser.fallback()
//         result(None)
// }

// ^expr ^expr ^expr term => Attribute([expr | expr | expr], term)
def tryAttributes(parser: Parser): Either[ParseError, List[Ast]] = {
  parser.enter()
  try boundary[Either[ParseError, List[Ast]]] {
      var res: List[Ast] = List()
      var endLoop = false
      while (!endLoop)
        if (parser.eatToken(lex.Tag.`^`)) {
          val properties = tryExpr(parser, ExprOption(precedence = 90)) match
            case Right(Some(node)) => node
            case Left(e) => boundary.break(Left(e))
            case _ => boundary.break(Left(parser.invalidTerm("an expression", "parsing an attribute")))
          res = res :+ properties
        } else { endLoop = true }

      Right(res)
    }
  finally parser.exit()
}

case class Rule(name: String, parserFn: Parser => ParseResult, delimiter: lex.Tag = lex.Tag.`,`)

def tryMulti(parser: Parser, opening: Option[lex.Tag], ctx: String, ruleS: Rule*): Either[ParseError, List[Ast]] = {
  import scala.util.boundary
  parser.enter()
  try boundary[Either[ParseError, List[Ast]]] {
      val rules: List[Rule] = ruleS.toList
      var isStatement = false
      var delimiter = lex.Tag.`,`
      var ruleName = "<?>"

      if (rules.isEmpty) throw new IllegalArgumentException("rules cannot be empty")
      val terminator: Option[lex.Tag] = opening.map {
        _ match
          case lex.Tag.`[` => lex.Tag.`]`
          case lex.Tag.`(` => lex.Tag.`)`
          case lex.Tag.`{` => lex.Tag.`}`
          case lex.Tag.`<` => lex.Tag.`>`
          case lex.Tag.`|` => lex.Tag.`|`
          case _ => throw new IllegalArgumentException("Invalid opening tag")
      }

      opening.foreach(x => if (!parser.eatToken(x)) boundary.break(Right(List())))

      var res: List[Ast] = List()
      val outerLoop = new Breaks
      // boundary {
      outerLoop.breakable {
        while (true) {
          var node: Option[Ast] = None

          val innerLoop = new Breaks
          var attributes = tryAttributes(parser) match
            case Right(nodes) => nodes
            case Left(e) => boundary.break(Left(e))

          innerLoop.breakable {
            for (rule <- rules) rule.parserFn(parser) match {
              case Right(Some(n)) =>
                node = Some(attributes.foldRight(n)(Ast.Attribute(_, _).withSpan(parser.currentSpan())))
                delimiter = rule.delimiter
                ruleName = rule.name
                innerLoop.break()
              case Left(e) =>
                boundary.break(Left(e))
                parser.fallback()
              case _ => ()
            }
          }

          // println(s"outer: node = $node")
          node match
            case Some(n) =>
              isStatement = delimiter match {
                case lex.Tag.`;` => true
                case _ => false
              }
              res = res :+ n
            case None if terminator.isDefined && parser.peek(terminator.get) => outerLoop.break()
            case None if !terminator.isDefined => outerLoop.break()
            case _ => boundary.break(Left(parser.invalidTerm(rules.map(_.name).mkString(" or "), ctx)))
          val nodeSrc = parser.srcContentT(parser.getToken(node.get.span.end))
          if (isStatement && nodeSrc.length() > 0 && nodeSrc.last == '}' || delimiter == lex.Tag.invalid) {
            parser.eatToken(delimiter) // 语句模式下，如果以}结尾，可选择性跳过分隔符
          } else {
            // 非语句模式下，必须有分隔符或者已到达终止符
            terminator match
              case Some(tag) =>
                if (parser.peek(tag)) outerLoop.break()
                else if (!parser.eatToken(delimiter)) { boundary.break(Left(parser.unexpectedToken(delimiter, ctx))) }
              case None => if (!parser.eatToken(delimiter)) { outerLoop.break() }
          }

          terminator.foreach(tag => if (parser.peek(tag)) outerLoop.break())
        }
      }
      // }

      terminator.foreach { tag =>
        if (!parser.eatToken(tag)) { boundary.break(Left(ParseError.UnexpectedToken(tag, parser.peekToken(), ctx))) }
      }

      Right(res)
    }
  finally parser.exit()
}

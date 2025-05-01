package parse

import scala.util.control.Breaks._
import scala.util.control.Breaks
import scala.util.boundary

type ParseResult = Either[ParseError, Option[AstNode]]

def result(None: Option[AstNode]): ParseResult = Right(None)

def result(node: AstNode): ParseResult = Right(Some(node))

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
def tryLiteral(parser: Parser): ParseResult = withCtx(parser) {
  val token = parser.peekToken()
  token.tag match {
    case lex.Tag.int => createValueNode(parser, token, Tag.int, _.toInt)
    case lex.Tag.real => createValueNode(parser, token, Tag.real, _.toFloat)
    case lex.Tag.str => createValueNode(parser, token, Tag.str, identity)
    case lex.Tag.char => createValueNode(parser, token, Tag.char, _.charAt(1))
    case lex.Tag.k_true => createValueNode(parser, token, Tag.bool, _ => true)
    case lex.Tag.k_false => createValueNode(parser, token, Tag.bool, _ => false)
    case lex.Tag.id => tryId(parser)
    case lex.Tag.k_null | lex.Tag.k_self | lex.Tag.k_itself | lex.Tag.k_Self =>
      parser.eatTokens(1)
      result(AstNode0(
        token.tag match {
          case lex.Tag.k_null => Tag.null_val
          case lex.Tag.k_self => Tag.self_val
          case lex.Tag.k_itself => Tag.itself
          case lex.Tag.k_Self => Tag.Self_type
          case _ => Tag.invalid
        },
        parser.currentSpan()
      ))
    case _ => result(None)
  }
}

def tryId(parser: Parser): ParseResult = withCtx(parser) {
  val token = parser.peekToken()
  if (token.tag == lex.Tag.id) {
    parser.eatTokens(1)
    result(AstNodeValue(Tag.id, parser.currentSpan(), parser.srcContentT(token)))
  } else { result(None) }
}

def trySymbol(parser: Parser): ParseResult = withCtx(parser) {
  if (parser.peek(lex.Tag.`.`, lex.Tag.id)) {
    parser.eatTokens(1)
    val id = parser.nextToken()
    result(
      AstNode1(Tag.symbol, parser.currentSpan(), AstNodeValue(Tag.id, parser.currentSpan(), parser.srcContentT(id)))
    )
  } else { result(None) }
}

def tryProperty(parser: Parser): ParseResult = withCtx(parser) {
  if (parser.peek(lex.Tag.`.`, lex.Tag.id)) {
    parser.eatTokens(1)
    val id = parser.nextToken()
    tryExpr(parser) match
      case Right(Some(expr)) => result(AstNode2(
          Tag.property,
          parser.currentSpan(),
          AstNodeValue(Tag.id, parser.currentSpan(), parser.srcContentT(id)),
          expr
        ))
      case _ =>
        parser.fallback()
        result(None)
  } else { result(None) }
}

def tryPropertyAssign(parser: Parser): ParseResult = withCtx(parser) {
  if (parser.peek(lex.Tag.`.`, lex.Tag.id, lex.Tag.`=`)) {
    parser.eatTokens(1)
    val id = tryId(parser) match {
      case Right(Some(node)) => node
      case _ => throw new IllegalArgumentException("Invalid property assignment")
    }
    if (!parser.eatToken(lex.Tag.`=`)) { result(parser.invalidTerm("an expression", "property assignment")) }
    else {
      tryExpr(parser) match
        case Right(Some(expr)) => result(AstNode2(Tag.property_assign, parser.currentSpan(), id, expr))
        case _ =>
          parser.fallback()
          result(None)
    }
  } else { result(None) }
}

// ^expr term
def tryAttribute(parser: Parser, followRule: Parser => ParseResult): ParseResult = withCtx(parser, Some(lex.Tag.`^`)) {
  boundary {
    val attr = tryExpr(parser, ExprOption(precedence = 90)) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(None))

    val term = followRule(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(None))

    result(AstNode2(Tag.attribute, parser.currentSpan(), attr, term))
  }
}

case class Rule(tag: Tag, parser_fn: Parser => ParseResult, delimiter: lex.Tag = lex.Tag.`,`)

def tryPrefixTerm(parser: Parser, tag: Tag, prefixToken: lex.Tag, followRule: Parser => ParseResult): ParseResult =
  withCtx(parser, Some(prefixToken)) {
    followRule(parser) match
      case Right(Some(node)) => result(AstNode1(tag, parser.currentSpan(), node))
      case Left(e) => result(e)
      case _ =>
        parser.fallback()
        result(None)
  }

// ^expr ^expr ^expr term => Attribute([expr | expr | expr], term)
def tryAttributes(parser: Parser): Either[ParseError, List[AstNode]] = {
  parser.enter()
  try boundary[Either[ParseError, List[AstNode]]] {
      var res: List[AstNode] = List()
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

def tryMulti(parser: Parser, opening: Option[lex.Tag], ctx: String, ruleS: Rule*): Either[ParseError, List[AstNode]] = {
  import scala.util.boundary
  parser.enter()
  try boundary[Either[ParseError, List[AstNode]]] {
      val rules: List[Rule] = ruleS.toList
      var isStatement = false
      var delimiter = lex.Tag.`,`
      var rule_name = "<?>"

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

      var res: List[AstNode] = List()
      val outerLoop = new Breaks
      // boundary {
      outerLoop.breakable {
        while (true) {
          var node: Option[AstNode] = None

          val innerLoop = new Breaks
          innerLoop.breakable {
            for (rule <- rules)
              var attributes = tryAttributes(parser) match
                case Right(nodes) => nodes
                case Left(e) => boundary.break(Left(e))

              rule.parser_fn(parser) match {
                case Right(Some(n)) =>
                  // BUG: 我不知道为什么，这对statement无效
                  node = Some(attributes.foldRight(n)(AstNode2(Tag.attribute, parser.currentSpan(), _, _)))
                  attributes.foreach(node => println(node.toString()))
                  delimiter = rule.delimiter
                  rule_name = rule.tag.toString
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
            case _ => boundary.break(Left(parser.invalidTerm(rules.map(_.tag).mkString(" or "), ctx)))

          if (
            isStatement && parser.srcContentT(parser.getToken(node.get.span.end)).last == '}' ||
            delimiter == lex.Tag.invalid
          ) {
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

// 新增辅助函数
private inline def createValueNode[T](
    parser: Parser,
    token: lex.Token,
    tag: Tag,
    transform: String => T
): ParseResult = {
  parser.eatTokens(1)
  result(AstNodeValue(tag, parser.currentSpan(), transform(parser.srcContentT(token))))
}

package parse

import scala.util.boundary
import scala.util.control.Breaks
import scala.collection.mutable

// 通用前缀术语解析函数
private def tryPrefixTerm(parser: Parser, tag: Tag, keywordTag: lex.Tag, tryFunc: Parser => ParseResult): ParseResult =
  withCtx(parser, Some(keywordTag)) {
    boundary[ParseResult] {
      val expr = tryFunc(parser) match
        case Right(Some(node)) => node
        case Left(err) => boundary.break(result(err))
        case _ => boundary.break(result(parser.invalidTerm("expression", s"parsing an expression after $keywordTag")))

      // 根据不同标签创建对应的 Ast 节点
      val termNode = tag match {
        case Tag.asserts => Ast.Asserts(expr)
        case Tag.assumes => Ast.Assumes(expr)
        case Tag.axiom => Ast.Axiom(expr)
        case Tag.invariant => Ast.Invariant(expr)
        case Tag.decreases => Ast.Decreases(expr)
        case _ => expr // 默认情况，不应该发生
      }

      result(termNode.withSpan(parser.currentSpan()))
    }
  }

def tryStatement(parser: Parser): ParseResult = withCtx(parser) {
  val token = parser.peekToken()
  token.tag match
    case lex.Tag.k_const => tryDecl(parser, Tag.const_decl, lex.Tag.k_const)
    case lex.Tag.k_let => tryDecl(parser, Tag.let_decl, lex.Tag.k_let)

    case lex.Tag.k_asserts => tryPrefixTerm(parser, Tag.asserts, lex.Tag.k_asserts, tryExpr)
    case lex.Tag.k_assumes => tryPrefixTerm(parser, Tag.assumes, lex.Tag.k_assumes, tryExpr)
    case lex.Tag.k_axiom => tryPrefixTerm(parser, Tag.axiom, lex.Tag.k_axiom, tryExpr)
    case lex.Tag.k_invariant => tryPrefixTerm(parser, Tag.invariant, lex.Tag.k_invariant, tryExpr)
    case lex.Tag.k_decreases => tryPrefixTerm(parser, Tag.decreases, lex.Tag.k_decreases, tryExpr)

    case lex.Tag.k_return => tryReturnStatement(parser)
    case lex.Tag.k_resume => tryResumeStatement(parser)

    case lex.Tag.k_break => tryJumpStatement(parser, Tag.break_statement, lex.Tag.k_break)
    case lex.Tag.k_continue => tryJumpStatement(parser, Tag.continue_statement, lex.Tag.k_continue)

    case lex.Tag.k_when => tryWhenStatement(parser)
    case lex.Tag.k_if => tryIfStatement(parser)

    case lex.Tag.k_while => tryWhileLoop(parser)
    case lex.Tag.k_for => tryForLoop(parser)

    case lex.Tag.k_use => tryUseStatement(parser)

    case _ => tryExprStatement(parser)
}

def tryFileScope(parser: Parser): ParseResult = withCtx(parser) {
  tryMulti(
    parser,
    None,
    "parsing a file scope",
    Rule("definition", tryDefinition, lex.Tag.`;`),
    Rule("statement", tryStatement, lex.Tag.`;`)
  ) match
    case Left(err) => result(err)
    case Right(nodes) => result(Ast.FileScope(nodes).withSpan(parser.currentSpan()))
}

// def tryQualifiedBlock(parser: Parser, tag: Tag, keywordTag: lex.Tag): ParseResult = withCtx(parser, Some(keywordTag)) {
//   boundary[ParseResult] {
//     var block: AstNode = tryBlock(parser) match
//       case Right(Some(b)) => b
//       case Left(err) => boundary.break(result(err))
//       case Right(None) => boundary
//           .break(result(parser.invalidTerm("block", "parsing a block after " + keywordTag.toString)))

//     result(AstNode1(tag, parser.currentSpan(), block))
//   }
// }

def tryBlock(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`{`), true) {
  tryMulti(
    parser,
    Some(lex.Tag.`{`),
    "parsing a block of statements",
    Rule("definition", tryDefinition, lex.Tag.`;`),
    Rule("statement", tryStatement, lex.Tag.`;`)
  ) match
    case Left(err) => result(err)
    case Right(nodes) => result(Ast.Block(nodes).withSpan(parser.currentSpan()))
}

private def tryDecl(parser: Parser, tag: Tag, keywordTag: lex.Tag): ParseResult = withCtx(parser, Some(keywordTag)) {
  boundary[ParseResult] {
    var attrs: mutable.Map[String, Ast] = mutable.Map()
    if (parser.eatToken(lex.Tag.k_async)) attrs += ("flurry_async" -> Ast.Bool(true).withSpan(parser.currentSpan()))
    if (parser.eatToken(lex.Tag.k_handles)) attrs += ("flurry_handles" -> Ast.Bool(true).withSpan(parser.currentSpan()))

    val pattern = tryPattern(parser) match
      case Right(Some(id)) => id
      case Left(err) => boundary.break(result(err))
      case _ => Ast.Id("").withSpan(parser.currentSpan())

    var typeNode: Ast =
      if (parser.eatToken(lex.Tag.`:`)) tryExpr(parser) match
        case Right(Some(t)) => t
        case Right(None) => Ast.Id("").withSpan(parser.currentSpan())
        case Left(err) => boundary.break(result(err))
      else Ast.Id("").withSpan(parser.currentSpan())

    var init: Ast =
      if (parser.eatToken(lex.Tag.`=`)) tryExpr(parser) match
        case Right(Some(expr)) => expr
        case Right(None) => Ast.Id("").withSpan(parser.currentSpan())
        case Left(err) => boundary.break(result(err))
      else Ast.Invalid.withSpan(parser.currentSpan())

    val declNode = tag match
      case Tag.const_decl => Ast.ConstDecl(pattern, typeNode, init)
      case Tag.let_decl => Ast.LetDecl(pattern, typeNode, init)
      case _ => Ast.Invalid

    if (attrs.nonEmpty)
      result(Ast.Attribute(Ast.Object(attrs.map(Ast.Property(_, _)).toList), declNode).withSpan(parser.currentSpan()))
    else result(declNode.withSpan(parser.currentSpan()))
  }
}

private def tryJumpStatement(parser: Parser, tag: Tag, keywordTag: lex.Tag): ParseResult =
  withCtx(parser, Some(keywordTag)) {
    boundary[ParseResult] {
      val label = parser.eatId() match
        case Some(id) => Ast.Id(id).withSpan(parser.currentSpan())
        case None => Ast.Invalid

      val guardExpr =
        if (parser.eatToken(lex.Tag.k_if)) tryExpr(parser) match
          case Right(Some(expr)) => expr
          case Right(None) => Ast.Invalid
          case Left(e) => boundary.break(result(e))
        else Ast.Invalid

      val jumpNode = tag match
        case Tag.break_statement => Ast.BreakStatement(label, guardExpr)
        case Tag.continue_statement => Ast.ContinueStatement(label, guardExpr)
        case _ => throw new IllegalArgumentException("reached unreachable code")

      result(jumpNode.withSpan(parser.currentSpan()))
    }
  }

def tryExprStatement(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    // 先尝试解析表达式
    val expr = tryExpr(parser) match
      case Right(Some(node)) => node
      case Right(None) => boundary.break(result(None))
      case Left(error) => boundary.break(result(error))

    // 定义处理赋值语句的辅助函数
    def handleAssignment(assignTag: Tag, opName: String): ParseResult = {
      parser.eatTokens(1)
      val right = tryExpr(parser) match
        case Right(Some(node)) => node
        case Left(error) => boundary.break(result(error))
        case _ => boundary
            .break(result(parser.invalidTerm("expression", s"missing right operand in $opName assignment")))

      // 根据赋值类型创建相应的 Ast 节点
      val assignNode = assignTag match {
        case Tag.assign => Ast.Assign(expr, right)
        case Tag.assign_add => Ast.Assign(expr, Ast.Add(expr, right).withSpan(parser.currentSpan()))
        case Tag.assign_sub => Ast.Assign(expr, Ast.Sub(expr, right).withSpan(parser.currentSpan()))
        case Tag.assign_mul => Ast.Assign(expr, Ast.Mul(expr, right).withSpan(parser.currentSpan()))
        case Tag.assign_div => Ast.Assign(expr, Ast.Div(expr, right).withSpan(parser.currentSpan()))
        case Tag.assign_mod => Ast.Assign(expr, Ast.Mod(expr, right).withSpan(parser.currentSpan()))
        case _ => expr // 默认情况，不应该发生
      }

      result(assignNode.withSpan(parser.currentSpan()))
    }

    // 根据下一个标记判断是哪种类型的语句
    parser.peekToken().tag match {
      case lex.Tag.`=` => handleAssignment(Tag.assign, "")
      case lex.Tag.`+=` => handleAssignment(Tag.assign_add, "+=")
      case lex.Tag.`-=` => handleAssignment(Tag.assign_sub, "-=")
      case lex.Tag.`*=` => handleAssignment(Tag.assign_mul, "*=")
      case lex.Tag.`/=` => handleAssignment(Tag.assign_div, "/=")
      case lex.Tag.`%=` => handleAssignment(Tag.assign_mod, "%=")
      case _ =>
        // 纯表达式语句
        result(expr)
    }
  }
}

// return -> return expr? if_guard?
def tryReturnStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_return)) {
  boundary[ParseResult] {
    var expr: Ast = tryExpr(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => Ast.Invalid

    val guardExpr =
      if (parser.eatToken(lex.Tag.k_if)) tryExpr(parser) match
        case Right(Some(expr)) => expr
        case Right(None) => Ast.Invalid
        case Left(error) => boundary.break(result(error))
      else Ast.Invalid

    result(Ast.ReturnStatement(expr, guardExpr).withSpan(parser.currentSpan()))
  }
}

// resume -> resume expr? if_guard?
def tryResumeStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_resume)) {
  boundary[ParseResult] {
    var expr: Ast = tryExpr(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => Ast.Invalid

    val guardExpr =
      if (parser.eatToken(lex.Tag.k_if)) tryExpr(parser) match
        case Right(Some(expr)) => expr
        case Right(None) => Ast.Invalid
        case Left(error) => boundary.break(result(error))
      else Ast.Invalid

    result(Ast.ResumeStatement(expr, guardExpr).withSpan(parser.currentSpan()))
  }
}

// condition_branch -> expr => stmt | block
def tryConditionBranch(parser: Parser): ParseResult = withCtx(parser) {
  boundary[ParseResult] {
    val expr = tryExpr(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(None))

    if (!parser.eatToken(lex.Tag.`=>`)) boundary
      .break(result(parser.invalidTerm("=>", "parsing a conditional branch after expression")))

    val stmtOrBlock = tryStatement(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => tryBlock(parser) match
          case Right(Some(node)) => node
          case Left(error) => boundary.break(result(error))
          case _ => boundary.break(result(None))

    result(Ast.ConditionBranch(expr, stmtOrBlock).withSpan(parser.currentSpan()))
  }
}

// pattern_branch -> pattern => stmt | block
def tryPatternBranch(parser: Parser): ParseResult = withCtx(parser) {
  boundary[ParseResult] {
    val pattern = tryPattern(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(None))

    if (!parser.eatToken(lex.Tag.`=>`)) boundary
      .break(result(parser.invalidTerm("=>", "parsing a conditional branch after expression")))

    val stmtOrBlock = tryStatement(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => tryBlock(parser) match
          case Right(Some(node)) => node
          case Left(error) => boundary.break(result(error))
          case _ => boundary.break(result(None))

    result(Ast.PatternBranch(pattern, stmtOrBlock).withSpan(parser.currentSpan()))
  }
}

// catch_branch -> catch id => stmt | block
def tryCatchBranch(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_catch)) {
  boundary[ParseResult] {
    val errorName = tryId(parser) match
      case Right(Some(node)) => node.asInstanceOf[Ast.Id].name
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(None))

    if (!parser.eatToken(lex.Tag.`=>`)) boundary
      .break(result(parser.invalidTerm("=>", "parsing a conditional branch after expression")))

    val stmtOrBlock = tryStatement(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => tryBlock(parser) match
          case Right(Some(node)) => node
          case Left(error) => boundary.break(result(error))
          case _ => boundary.break(result(None))

    result(Ast.CatchBranch(errorName, stmtOrBlock).withSpan(parser.currentSpan()))
  }
}

// when -> when { condition_branch* }
def tryWhenStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_when)) {
  boundary[ParseResult] {
    val branches =
      tryMulti(parser, Some(lex.Tag.`{`), "parsing when branches", Rule("condition_branch", tryConditionBranch)) match
        case Right(nodes) => nodes
        case Left(error) => boundary.break(result(error))

    result(Ast.WhenStatement(branches).withSpan(parser.currentSpan()))
  }
}

// if -> if expr block else?
def tryIfStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_if)) {
  boundary[ParseResult] {
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(parser.invalidTerm("expression", "parsing condition expression in if statement")))

    if (parser.eatToken(lex.Tag.k_is)) boundary.break(parseIfIsOrIfMatch(parser, expr))

    // 解析块
    val block = tryBlock(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after if condition")))

    // 处理 else 子句
    var elseClause: Ast = null
    if (parser.eatToken(lex.Tag.k_else)) {
      // 尝试解析 else if
      elseClause = tryIfStatement(parser) match
        case Right(Some(node)) => node
        case Left(error) => boundary.break(result(error))
        case _ =>
          // 否则，解析普通 else 块
          tryBlock(parser) match
            case Right(Some(node)) => node
            case Left(error) => boundary.break(result(error))
            case _ => boundary
                .break(result(parser.invalidTerm("block or if", "expected a block or if statement after else")))
    }

    result(Ast.IfStatement(expr, block, elseClause).withSpan(parser.currentSpan()))
  }
}

private def parseIfIsOrIfMatch(parser: Parser, expr: Ast): ParseResult = boundary[ParseResult] {
  if (parser.eatToken(lex.Tag.k_do))
    tryMulti(parser, Some(lex.Tag.`{`), "parsing if branches", Rule("pattern_branch", tryPatternBranch)) match
      case Right(nodes) => result(Ast.IfMatch(expr, nodes).withSpan(parser.currentSpan()))
      case Left(error) => boundary.break(result(error))
  else tryPattern(parser) match
    case Right(Some(pattern)) =>
      if (!parser.eatToken(lex.Tag.k_do)) boundary
        .break(result(parser.invalidTerm("do", "expected 'do' after the pattern")))

      tryBlock(parser) match
        case Right(Some(block)) =>
          val else_ =
            if (parser.eatToken(lex.Tag.k_else)) {
              tryBlock(parser) match
                case Right(Some(node)) => node
                case Left(error) => boundary.break(result(error))
                case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after else")))
            } else Ast.Invalid

          result(Ast.IfIsMatch(expr, pattern, block, else_).withSpan(parser.currentSpan()))
        case Left(error) => boundary.break(result(error))
        case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after the pattern")))

    case Left(error) => boundary.break(result(error))
    case _ => boundary.break(result(parser.invalidTerm("pattern", "expected a pattern after is")))
}

// use_statement -> use expr
def tryUseStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_use)) {
  boundary[ParseResult] {
    val expr = tryExpr(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(parser.invalidTerm("expression", "parsing expression after 'use'")))

    result(Ast.UseStatement(expr).withSpan(parser.currentSpan()))
  }
}

// : label
def tryLabel(parser: Parser): Either[ParseError, String] =
  if (parser.eatToken(lex.Tag.`:`)) {
    val next = parser.peekToken()
    if (next.tag == lex.Tag.id) Right(parser.srcContentT(parser.nextToken()))
    else Left(parser.invalidTerm("label", "expected a label after ':'"))
  } else Right("")

// while_loop -> while:label? expr block
def tryWhileLoop(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_while)) {
  boundary[ParseResult] {
    val label: String = tryLabel(parser) match
      case Right(label) => label
      case Left(e) => boundary.break(result(e))

    // 循环条件表达式
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected a condition expression for while")))

    if (parser.eatToken(lex.Tag.k_is)) boundary.break(parseWhileIsOrWhileMatch(label, parser, expr))

    // 处理普通 while 循环
    val block = tryBlock(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after while condition")))

    // 创建普通while循环AST节点
    result(Ast.WhileLoop(label, expr, block).withSpan(parser.currentSpan()))
  }
}

private def parseWhileIsOrWhileMatch(label: String, parser: Parser, expr: Ast): ParseResult = boundary[ParseResult] {
  if (parser.eatToken(lex.Tag.k_do))
    tryMulti(parser, Some(lex.Tag.`{`), "parsing while branches", Rule("pattern_branch", tryPatternBranch)) match
      case Right(nodes) => result(Ast.WhileMatch(label, expr, nodes).withSpan(parser.currentSpan()))
      case Left(error) => boundary.break(result(error))
  else tryPattern(parser) match
    case Right(Some(pattern)) =>
      if (!parser.eatToken(lex.Tag.k_do)) boundary
        .break(result(parser.invalidTerm("do", "expected 'do' after the pattern")))

      tryBlock(parser) match
        case Right(Some(block)) => result(Ast.WhileIsMatch(label, expr, pattern, block).withSpan(parser.currentSpan()))
        case Left(error) => boundary.break(result(error))
        case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after the pattern")))

    case Left(error) => boundary.break(result(error))
    case _ => boundary.break(result(parser.invalidTerm("pattern", "expected a pattern after is")))
}

// for_loop -> for label? pattern in expr block
def tryForLoop(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_for)) {
  boundary[ParseResult] {
    val label = tryLabel(parser) match
      case Right(l) => l
      case Left(e) => boundary.break(result(e))

    val pattern = tryPattern(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("pattern", "expected a pattern after for loop")))

    if (!parser.eatToken(lex.Tag.k_in)) boundary
      .break(result(parser.invalidTerm("in", "expected 'in' after for loop pattern")))

    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after for loop")))

    val block = tryBlock(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after for loop expressions")))

    // 创建for循环AST节点
    result(Ast.ForLoop(label, pattern, expr, block).withSpan(parser.currentSpan()))
  }
}

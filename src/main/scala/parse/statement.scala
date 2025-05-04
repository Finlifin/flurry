package parse

import scala.util.boundary
import scala.util.control.Breaks

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
    Rule(Tag.definition, tryDefinition, lex.Tag.`;`),
    Rule(Tag.statement, tryStatement, lex.Tag.`;`)
  ) match
    case Left(err) => result(err)
    case Right(nodes) => result(AstNodeN(Tag.file_scope, parser.currentSpan(), nodes))
}

def tryQualifiedBlock(parser: Parser, tag: Tag, keywordTag: lex.Tag): ParseResult = withCtx(parser, Some(keywordTag)) {
  boundary[ParseResult] {
    var block: AstNode = tryBlock(parser) match
      case Right(Some(b)) => b
      case Left(err) => boundary.break(result(err))
      case Right(None) => boundary
          .break(result(parser.invalidTerm("block", "parsing a block after " + keywordTag.toString)))

    result(AstNode1(tag, parser.currentSpan(), block))
  }
}

def tryBlock(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`{`), true) {
  tryMulti(
    parser,
    Some(lex.Tag.`{`),
    "parsing a block of statements",
    Rule(Tag.definition, tryDefinition, lex.Tag.`;`),
    Rule(Tag.statement, tryStatement, lex.Tag.`;`)
  ) match
    case Left(err) => result(err)
    case Right(nodes) => result(AstNodeN(Tag.block, parser.currentSpan(), nodes))
}

// BUG
private def tryDecl(parser: Parser, tag: Tag, keywordTag: lex.Tag): ParseResult = withCtx(parser, Some(keywordTag)) {
  boundary[ParseResult] {
    val pattern = tryPattern(parser) match
      case Right(Some(id)) => id
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    var typeNode: AstNode =
      if (parser.eatToken(lex.Tag.`:`)) tryExpr(parser) match
        case Right(Some(t)) => t
        case Right(None) => AstNode0(Tag.invalid, parser.currentSpan())
        case Left(err) => boundary.break(result(err))
      else AstNode0(Tag.invalid, parser.currentSpan())

    var init: AstNode =
      if (parser.eatToken(lex.Tag.`=`)) tryExpr(parser) match
        case Right(Some(expr)) => expr
        case Right(None) => AstNode0(Tag.invalid, parser.currentSpan())
        case Left(err) => boundary.break(result(err))
      else AstNode0(Tag.invalid, parser.currentSpan())

    result(AstNode3(tag, parser.currentSpan(), pattern, typeNode, init))
  }
}

private def tryJumpStatement(parser: Parser, tag: Tag, keywordTag: lex.Tag): ParseResult =
  withCtx(parser, Some(keywordTag)) {
    boundary[ParseResult] {
      val label = tryId(parser) match
        case Right(Some(id)) => id
        case _ => AstNode0(Tag.invalid, parser.currentSpan())

      var guard: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
      if (parser.eatToken(lex.Tag.k_if)) {
        guard = tryExpr(parser) match
          case Right(Some(expr)) => expr
          case Right(None) => AstNode0(Tag.invalid, parser.currentSpan())
          case Left(err) => boundary.break(result(err))
      }
      result(AstNode2(tag, parser.currentSpan(), label, guard))
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

      result(AstNode2(assignTag, parser.currentSpan(), expr, right))
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
        result(AstNode1(Tag.expr_statement, parser.currentSpan(), expr))
    }
  }
}

// return -> return expr? if_guard?
def tryReturnStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_return)) {
  boundary[ParseResult] {
    var expr: AstNode = tryExpr(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    var guard: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    if (parser.eatToken(lex.Tag.k_if)) {
      guard = tryExpr(parser) match
        case Right(Some(node)) => node
        case Left(error) => boundary.break(result(error))
        case _ => boundary
            .break(result(parser.invalidTerm("condition expression", "parsing a guard expression after return")))
    }
    result(AstNode2(Tag.return_statement, parser.currentSpan(), expr, guard))
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

    result(AstNode2(Tag.condition_branch, parser.currentSpan(), expr, stmtOrBlock))
  }
}

// when -> when { condition_branch* }
def tryWhenStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_when)) {
  boundary[ParseResult] {
    val branches =
      tryMulti(parser, Some(lex.Tag.`{`), "parsing when branches", Rule(Tag.condition_branch, tryConditionBranch)) match
        case Right(nodes) => nodes
        case Left(error) => boundary.break(result(error))

    result(AstNodeN(Tag.when_statement, parser.currentSpan(), branches))
  }
}

// if_is_match -> if expr is do { branch* }
// if_is_pattern -> if expr is pattern do block (else (if | block))?
// if -> if expr block else?
def tryIfStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_if)) {
  boundary[ParseResult] {
    val expr = tryExpr(parser, ExprOption(true)) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(parser.invalidTerm("expression", "parsing condition expression in if statement")))

    // 处理 if expr is do { branch* } 情况
    if (parser.peek(lex.Tag.k_is, lex.Tag.k_do)) {
      parser.eatTokens(2)

      val branches = tryMulti(parser, Some(lex.Tag.`{`), "parsing pattern branches", Rule(Tag.branch, tryBranch)) match
        case Right(nodes) => nodes
        case Left(error) => boundary.break(result(error))

      boundary.break(result(AstNodeL(Tag.if_is_match, parser.currentSpan(), expr, branches)))
    }

    // 处理 if expr is pattern do 情况
    var pattern: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    if (parser.eatToken(lex.Tag.k_is))
      pattern = tryPattern(parser) match
        case Right(Some(node)) => node
        case Left(error) => boundary.break(result(error))
        case _ => boundary.break(result(parser.invalidTerm("pattern", "missing pattern after `is` in if statement")))

      if (!parser.eatToken(lex.Tag.k_do)) {
        boundary.break(result(parser.invalidTerm("do", "missing `do` keyword after the pattern")))
      }

    // 解析块
    val block = tryBlock(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after if condition")))

    // 处理 else 子句
    var elseClause: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
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

    // 根据是否有模式匹配返回不同类型的节点
    if (pattern.getTag == Tag.invalid) {
      result(AstNode3(Tag.if_statement, parser.currentSpan(), expr, block, elseClause))
    } else { result(AstNode4(Tag.if_is_pattern, parser.currentSpan(), expr, pattern, block, elseClause)) }
  }
}

// branch -> pattern => statement | block
def tryBranch(parser: Parser): ParseResult = withCtx(parser) {
  boundary[ParseResult] {
    val pattern = tryPattern(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(None))

    if (!parser.eatToken(lex.Tag.`=>`)) boundary.break(result(parser.invalidTerm("=>", "expected `=>` after pattern")))

    val stmtOrBlock = tryStatement(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => tryBlock(parser) match
          case Right(Some(node)) => node
          case Left(error) => boundary.break(result(error))
          case _ => boundary.break(result(None))

    result(AstNode2(Tag.branch, parser.currentSpan(), pattern, stmtOrBlock))
  }
}

// catch_branch -> catch e => statement | block
def tryCatchBranch(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_catch)) {
  boundary[ParseResult] {
    val errorName = tryId(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    if (!parser.eatToken(lex.Tag.`=>`)) boundary
      .break(result(parser.invalidTerm("=>", "expected `=>` after catch clause")))

    val stmtOrBlock = tryStatement(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => tryBlock(parser) match
          case Right(Some(node)) => node
          case Left(error) => boundary.break(result(error))
          case _ => boundary.break(result(None))

    result(AstNode2(Tag.catch_branch, parser.currentSpan(), errorName, stmtOrBlock))
  }
}

// use_statement -> use mod_path
// mod_path ->
//     id
//     | path_select
//     | path_select_all
//     | path_select_multi
//     | super_path
//     | package_path
//     | path_as_bind
// path_select -> mod_path . id
// path_select_multi -> mod_path . { mod_path*}
// path_select_all -> mod_path . *
// super_path -> (.)+ mod_path
// package_path -> @ mod_path
// path_as_bind -> mod_path as id
// exclude_path -> not id
def tryUseStatement(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_use)) {
  boundary[ParseResult] {
    val path = tryPath(parser) match
      case Right(Some(node)) => node
      case Left(error) => boundary.break(result(error))
      case _ => boundary.break(result(parser.invalidTerm("mod_path", "parsing module path after 'use'")))

    result(AstNode1(Tag.use_statement, parser.currentSpan(), path))
  }
}

def tryPath(parser: Parser): ParseResult = withCtx(parser)(tryPathPratt(parser, 0))

// 模块路径操作符表
object PathOpTable {
  private val DEFAULT_OP_INFO = OpInfo(-1, Tag.invalid)

  val table: Map[lex.Tag, OpInfo] =
    Map(lex.Tag.`.` -> OpInfo(10, Tag.path_select), lex.Tag.k_as -> OpInfo(20, Tag.path_as_bind))

  // 获取操作符信息，如果不存在则返回默认值
  def getOpInfo(tag: lex.Tag): OpInfo = table.getOrElse(tag, DEFAULT_OP_INFO)
}

// Pratt解析器用于模块路径
def tryPathPratt(parser: Parser, minPrec: Int): ParseResult = boundary {
  // 尝试解析标识符作为左操作数
  val left = parser.peekToken().tag match
    case lex.Tag.id => tryId(parser) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(None))
    case lex.Tag.`@` => tryPrefixTerm(parser, Tag.package_path, lex.Tag.`@`, tryPath) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(None))
    case lex.Tag.`.` => tryPrefixTerm(parser, Tag.super_path, lex.Tag.`.`, tryPath) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(None))
    case lex.Tag.k_not => tryPrefixTerm(parser, Tag.exclude_path, lex.Tag.k_not, tryPath) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(None))
    case _ => boundary.break(result(parser.invalidTerm("mod_path", "parsing a module path")))

  // 循环处理操作符和右操作数
  var currentLeft = left
  val loop = new Breaks
  loop.breakable {
    while (true) {
      val token = parser.peekToken()
      val opInfo = PathOpTable.getOpInfo(token.tag)

      // 如果操作符无效或优先级太低，则退出循环
      if (opInfo.tag == Tag.invalid || opInfo.prec < minPrec) { loop.break }

      // 尝试解析后缀表达式
      tryPostfixPath(parser, token.tag, currentLeft) match
        case Right(Some(node)) => currentLeft = node // 更新当前操作数为后缀表达式
        // 继续下一次迭代
        case Left(e) => boundary.break(Left(e))
        case _ =>
          // 消耗操作符标记
          parser.eatTokens(1)

          // 解析右操作数（递归调用，使用更高优先级）
          val right = tryPathPratt(parser, opInfo.prec + 1) match
            case Right(Some(node)) => node
            case _ => boundary
                .break(result(parser.invalidTerm("path expression", "missing right operand for path operator")))

          // 创建二元操作符节点
          currentLeft = AstNode2(opInfo.tag, parser.currentSpan(), currentLeft, right)
    }
  }

  // 返回最终构建的路径表达式
  result(currentLeft)
}

// 尝试解析后缀路径表达式
def tryPostfixPath(parser: Parser, tag: lex.Tag, left: AstNode): ParseResult = boundary {
  var res: Option[AstNode] = None
  tag match {
    case lex.Tag.`.` =>
      // 选择全部
      if (parser.peek(lex.Tag.`.`, lex.Tag.`*`)) {
        parser.eatTokens(2)
        res = Some(AstNode1(Tag.path_select_all, parser.currentSpan(), left))
      }
      // 选择多个
      else if (parser.peek(lex.Tag.`.`, lex.Tag.`{`)) {
        parser.eatTokens(1)
        val nodes =
          tryMulti(parser, Some(lex.Tag.`{`), "parsing multiple module paths", Rule(Tag.mod_path, tryPath)) match
            case Right(nodes) => nodes
            case Left(e) => boundary.break(result(e))

        res = Some(AstNodeL(Tag.path_select_multi, parser.currentSpan(), left, nodes))
      }
      // 单个选择
      else if (parser.peek(lex.Tag.`.`, lex.Tag.id)) {
        parser.eatTokens(1)
        val id = tryId(parser) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(None))

        res = Some(AstNode2(Tag.path_select, parser.currentSpan(), left, id))
      } else { boundary.break(result(None)) }

    case lex.Tag.k_as =>
      if (!parser.peek(lex.Tag.k_as, lex.Tag.id)) { boundary.break(result(None)) }

      parser.eatTokens(1)
      val id = tryId(parser) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(None))

      res = Some(AstNode2(Tag.path_as_bind, parser.currentSpan(), left, id))

    // exclude_path -> not id
    case lex.Tag.k_not =>
      if (!parser.peek(lex.Tag.k_not, lex.Tag.id)) { boundary.break(result(None)) }

      parser.eatTokens(1)
      val id = tryId(parser) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(None))

      res = Some(AstNode2(Tag.exclude_path, parser.currentSpan(), left, id))

    case _ => boundary.break(result(None))
  }

  result(res.get)
}

// -- 基本循环语句
// for_loop -> for label? pattern in expr block
def tryForLoop(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_for)) {
  boundary[ParseResult] {
    // 可选的循环标签
    var label: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    if (parser.eatToken(lex.Tag.`:`)) {
      label = tryId(parser) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => AstNode0(Tag.invalid, parser.currentSpan())
    }

    // 循环模式
    val pattern = tryPattern(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("pattern", "expected a pattern after for")))

    // in 关键字
    if (!parser.eatToken(lex.Tag.k_in)) {
      boundary.break(result(parser.invalidTerm("in", "expected `in` keyword after pattern")))
    }

    // 遍历表达式
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected a collection expression after in")))

    // 循环主体块
    val block = tryBlock(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after for expression")))

    // 创建for循环AST节点
    result(AstNode4(Tag.for_loop, parser.currentSpan(), label, pattern, expr, block))
  }
}

// while_is_match -> while label? expr is do { branch* }
// while_is_pattern -> while label? expr is pattern do block
// while_loop -> while label? expr block
def tryWhileLoop(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_while)) {
  boundary[ParseResult] {
    // 可选的循环标签
    var label: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    if (parser.eatToken(lex.Tag.`:`)) {
      label = tryId(parser) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => AstNode0(Tag.invalid, parser.currentSpan())
    }

    // 循环条件表达式
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected a condition expression for while")))

    if (parser.peek(lex.Tag.k_is, lex.Tag.k_do)) {
      parser.eatTokens(2)

      // while_is_match
      val branches = tryMulti(parser, Some(lex.Tag.`{`), "parsing pattern branches", Rule(Tag.branch, tryBranch)) match
        case Right(nodes) => nodes
        case Left(e) => boundary.break(result(e))

      boundary.break(result(AstNode3(
        Tag.while_is_match,
        parser.currentSpan(),
        label,
        expr,
        AstNodeN(Tag.branches, parser.currentSpan(), branches)
      )))
    }

    //  while_is_pattern do
    if (parser.eatToken(lex.Tag.k_is)) {
      val pattern = tryPattern(parser, PatternOption(noRecordCall = true)) match
        case Right(Some(node)) =>
          if (!parser.eatToken(lex.Tag.k_do)) {
            boundary.break(result(parser.invalidTerm("do", "expected `do` keyword after the pattern")))
          }
          node
        case Left(e) => boundary.break(result(e))
        case Right(None) => boundary
            .break(result(parser.invalidTerm("pattern", "expected a pattern after `is` in while statement")))

      // 解析循环主体块
      val block = tryBlock(parser) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after pattern")))

      // 创建while_is_pattern AST节点
      boundary.break(result(AstNode4(Tag.while_is_pattern, parser.currentSpan(), label, expr, pattern, block)))
    }

    // 处理普通 while 循环
    val block = tryBlock(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("block", "expected a block after while condition")))

    // 创建普通while循环AST节点
    result(AstNode3(Tag.while_loop, parser.currentSpan(), label, expr, block))
  }
}

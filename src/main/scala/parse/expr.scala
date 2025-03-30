package parse

import scala.util.boundary
import scala.util.chaining._
import scala.util.control.Breaks

class ExprOption(val noRecordCall: Boolean = false)

def tryExpr(parser: Parser): ParseResult = withCtx(parser)(tryPratt(parser, 0, ExprOption()))

def tryExpr(parser: Parser, opt: ExprOption): ParseResult = withCtx(parser)(tryPratt(parser, 0, opt))

/** 操作符信息，包含优先级和对应的AST标签 */
case class OpInfo(prec: Int, tag: Tag)

/** 操作符表，将词法标记映射到操作符信息 */
object OpTable {
  private val DEFAULT_OP_INFO = OpInfo(-1, Tag.invalid)

  val table: Map[lex.Tag, OpInfo] = Map(
    lex.Tag.`==>` -> OpInfo(10, Tag.bool_implies),
    lex.Tag.k_or -> OpInfo(20, Tag.bool_or),
    lex.Tag.k_and -> OpInfo(30, Tag.bool_and),
    lex.Tag.`!=` -> OpInfo(40, Tag.bool_not_eq),
    lex.Tag.`==` -> OpInfo(40, Tag.bool_eq),
    lex.Tag.`>=` -> OpInfo(40, Tag.bool_gt_eq),
    lex.Tag.`_>_` -> OpInfo(40, Tag.bool_gt), // " > "
    lex.Tag.`<=` -> OpInfo(40, Tag.bool_lt_eq),
    lex.Tag.`_<_` -> OpInfo(40, Tag.bool_lt), // " < "
    lex.Tag.`:` -> OpInfo(40, Tag.type_with),
    lex.Tag.`:-` -> OpInfo(40, Tag.trait_bound),
    lex.Tag.k_matches -> OpInfo(40, Tag.bool_matches),
    lex.Tag.`_+_` -> OpInfo(60, Tag.add), // " + "
    lex.Tag.`_-_` -> OpInfo(60, Tag.sub), // " - "

    lex.Tag.`_/_` -> OpInfo(70, Tag.div), // " / "
    lex.Tag.`_*_` -> OpInfo(70, Tag.mul), // " * "
    lex.Tag.`_%_` -> OpInfo(70, Tag.mod), // " % "
    lex.Tag.`++` -> OpInfo(70, Tag.add_add),
    lex.Tag.`|` -> OpInfo(80, Tag.pipe),
    lex.Tag.`|>` -> OpInfo(80, Tag.pipe_prepend),
    lex.Tag.`(` -> OpInfo(90, Tag.call),
    lex.Tag.`[` -> OpInfo(90, Tag.index_call),
    lex.Tag.`{` -> OpInfo(90, Tag.record_call),
    lex.Tag.`<` -> OpInfo(90, Tag.diamond_call),
    lex.Tag.`#` -> OpInfo(90, Tag.effect_elimination),
    lex.Tag.`!` -> OpInfo(90, Tag.error_elimination),
    lex.Tag.`?` -> OpInfo(90, Tag.option_elimination),
    lex.Tag.k_match -> OpInfo(90, Tag.post_match),
    lex.Tag.`.` -> OpInfo(100, Tag.select),
    lex.Tag.`'` -> OpInfo(100, Tag.image),

    // 字面量扩展
    lex.Tag.id -> OpInfo(110, Tag.id)
  )

  /** 获取操作符信息，如果不存在则返回默认值 */
  def getOpInfo(tag: lex.Tag): OpInfo = table.getOrElse(tag, DEFAULT_OP_INFO)
}

/** 使用 Pratt 解析法解析表达式
  * @param parser
  *   解析器实例
  * @param minPrec
  *   最小优先级
  * @param opt
  *   表达式选项
  * @return
  *   解析结果
  */
def tryPratt(parser: Parser, minPrec: Int, opt: ExprOption): ParseResult = withCtx(parser) {
  boundary {
    // 尝试解析前缀表达式作为左操作数
    val left = tryPrefixExpr(parser) match
      case Right(Some(node)) => node
      case Right(None) => boundary.break(result(None))
      case Left(error) => boundary.break(result(error))

    // 循环处理操作符和右操作数
    var currentLeft = left
    val loop = new Breaks
    loop.breakable {
      while (true) {
        val token = parser.peekToken()
        val opInfo = OpTable.getOpInfo(token.tag)

        // 如果操作符无效或优先级太低，则退出循环
        if (opInfo.tag == Tag.invalid || opInfo.prec < minPrec) { loop.break }

        // 尝试解析后缀表达式
        tryPostfixExpr(parser, token.tag, currentLeft, opt) match
          case Right(Some(node)) => currentLeft = node // 更新当前操作数为后缀表达式
          case Left(ParseError.MeetRecordStart) => loop.break()
          case Left(e) => boundary.break(Left(e))
          case _ =>
            // 消耗操作符标记
            parser.eatTokens(1)

            // 解析右操作数（递归调用，使用更高优先级）
            val right = tryPratt(parser, opInfo.prec + 1, opt) match
              case Right(Some(node)) => node
              case _ => boundary
                  .break(result(parser.invalidTerm("expression", "parsing a right operand for binary operator")))

            // 创建二元操作符节点
            currentLeft = AstNode2(opInfo.tag, parser.currentSpan(), currentLeft, right)
      }
    }

    // 返回最终构建的表达式
    result(currentLeft)
  }
}

/** 尝试解析后缀表达式
  * @param parser
  *   解析器实例
  * @param tag
  *   当前标记的标签
  * @param left
  *   左侧表达式节点
  * @param opt
  *   表达式选项
  * @return
  *   解析结果
  */
def tryPostfixExpr(parser: Parser, tag: lex.Tag, left: AstNode, opt: ExprOption): ParseResult =
  // withCtx(parser) {
  boundary {
    var res: Option[AstNode] = None
    tag match {
      case lex.Tag.`(` =>
        // 函数调用
        val nodes = tryMulti(
          parser,
          Some(lex.Tag.`(`),
          "parsing a call",
          lex.Tag.`,`,
          Rule(Tag.expr, tryExpr),
          Rule(Tag.property, tryProperty)
          // Rule(Tag.expand_items, tryExpandItems)
        ) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))

        res = Some(AstNodeL(Tag.call, parser.currentSpan(), left, nodes))

      case lex.Tag.`<` =>
        // 泛型调用
        val nodes = tryMulti(
          parser,
          Some(lex.Tag.`<`),
          "parsing a diamond call",
          lex.Tag.`,`,
          Rule(Tag.property, tryProperty),
          Rule(Tag.expr, tryExpr)
        ) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))

        res = Some(AstNodeL(Tag.diamond_call, parser.currentSpan(), left, nodes))

      case lex.Tag.`{` =>
        // 记录调用
        if (opt.noRecordCall) boundary.break(result(ParseError.MeetRecordStart))

        val nodes = tryMulti(
          parser,
          Some(lex.Tag.`{`),
          "parsing a record call",
          lex.Tag.`,`,
          Rule(Tag.property, tryProperty)
          // Rule(Tag.expand_items, tryExpandItems)
        ) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))

        res = Some(AstNodeL(Tag.record_call, parser.currentSpan(), left, nodes))

      case lex.Tag.`[` =>
        // 索引调用
        parser.eatTokens(1)
        val indexExpr = tryExpr(parser) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidTerm("expression", "parsing an index expression")))

        if (!parser.eatToken(lex.Tag.`]`)) boundary
          .break(result(ParseError.UnexpectedToken(lex.Tag.`]`, parser.peekToken(), "closing an indexing expression")))

        res = Some(AstNode2(Tag.index_call, parser.currentSpan(), left, indexExpr))

      case lex.Tag.`.` =>
        // 点操作符相关
        parser.eatTokens(1)
        val next = parser.peekToken()

        next.tag match {
          case lex.Tag.`*` =>
            // 解引用
            parser.eatTokens(1)
            res = Some(AstNode1(Tag.deref, parser.currentSpan(), left))

          case lex.Tag.k_use =>
            // 处理器应用
            parser.eatTokens(1)
            if (!parser.eatToken(lex.Tag.`(`)) boundary.break(result(
              ParseError.UnexpectedToken(lex.Tag.`(`, parser.peekToken(), "parsing an handler expression")
            ))

            val handler = tryExpr(parser) match
              case Right(Some(node)) => node
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(parser.invalidTerm("expression", "expected a handler expression")))

            if (!parser.eatToken(lex.Tag.`)`)) boundary.break(result(
              ParseError
                .UnexpectedToken(lex.Tag.`)`, parser.peekToken(), "expected a `)` to close a handler expression")
            ))

            res = Some(AstNode2(Tag.handler_apply, parser.currentSpan(), left, handler))

          case lex.Tag.k_ref =>
            // 引用
            parser.eatTokens(1)
            res = Some(AstNode1(Tag.refer, parser.currentSpan(), left))

          case lex.Tag.k_dyn =>
            // 动态转换
            parser.eatTokens(1)
            if (!parser.eatToken(lex.Tag.`(`)) boundary
              .break(result(ParseError.UnexpectedToken(lex.Tag.`(`, parser.peekToken(), "parsing trait expression")))

            val trait_expr = tryExpr(parser) match
              case Right(Some(node)) => node
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(parser.invalidTerm("expression", "parsing a trait expression")))

            if (!parser.eatToken(lex.Tag.`)`)) boundary
              .break(result(ParseError.UnexpectedToken(lex.Tag.`)`, parser.peekToken(), "closing a trait expression")))

            res = Some(AstNode2(Tag.as_dyn, parser.currentSpan(), left, trait_expr))

          case lex.Tag.k_as =>
            // 类型转换
            parser.eatTokens(1)
            if (!parser.eatToken(lex.Tag.`(`)) boundary.break(result(
              ParseError.UnexpectedToken(lex.Tag.`(`, parser.peekToken(), "parsing type casting expression")
            ))

            val typeExpr = tryExpr(parser) match
              case Right(Some(node)) => node
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(parser.invalidTerm("type expression", "parsing type casting expression")))

            if (!parser.eatToken(lex.Tag.`)`)) boundary.break(result(
              ParseError.UnexpectedToken(lex.Tag.`)`, parser.peekToken(), "parsing type casting expression")
            ))

            res = Some(AstNode2(Tag.type_cast, parser.currentSpan(), left, typeExpr))

          case lex.Tag.`.` =>
            // 区间表达式
            if (parser.peek(lex.Tag.`.`, lex.Tag.`=`)) {
              parser.eatTokens(2)
              val end = tryExpr(parser, ExprOption(true)) match
                case Right(Some(node)) => node
                case Left(e) => boundary.break(result(e))
                case _ => boundary.break(result(parser.invalidTerm("expression", "parsing a range expression")))

              res = Some(AstNode2(Tag.range_from_to_inclusive, parser.currentSpan(), left, end))
            } else {
              parser.eatTokens(1)
              tryExpr(parser, ExprOption(true)) match {
                case Right(Some(end)) => res = Some(AstNode2(Tag.range_from_to, parser.currentSpan(), left, end))
                case Right(None) => res = Some(AstNode1(Tag.range_from, parser.currentSpan(), left))
                case Left(e) => boundary.break(result(e))
              }
            }

          case _ =>
            // 字段选择
            tryId(parser) match
              case Right(Some(id)) => res = Some(AstNode2(Tag.select, parser.currentSpan(), left, id))
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(parser.invalidTerm("identifier", "parsing symbol selection expression")))
        }

      case lex.Tag.`'` =>
        // 取像或柯里化调用
        parser.eatTokens(1)
        if (parser.peek(lex.Tag.`(`)) {
          // 柯里化调用
          val nodes = tryMulti(
            parser,
            Some(lex.Tag.`(`),
            "parsing a curry call",
            lex.Tag.`,`,
            Rule(Tag.expr, tryExpr),
            Rule(Tag.property, tryProperty)
            // Rule(Tag.expand_items, tryExpandItems)
          ) match
            case Right(nodes) => nodes
            case Left(e) => boundary.break(result(e))

          res = Some(AstNodeL(Tag.curry_call, parser.currentSpan(), left, nodes))
        } else {
          // 图像
          tryId(parser) match
            case Right(Some(id)) => res = Some(AstNode2(Tag.image, parser.currentSpan(), left, id))
            case Left(e) => boundary.break(result(e))
            case _ => boundary.break(result(parser.invalidTerm("identifier", "parsing an image selection expression")))
        }

      case lex.Tag.`#` =>
        // 效果发射或消除
        parser.eatTokens(1)
        if (!parser.peek(lex.Tag.`{`)) { res = Some(AstNode1(Tag.effect_emit, parser.currentSpan(), left)) }
        else {
          // 效果消除，需要解析分支
          // val branches = tryBlock(parser, Rule(Tag.branch, tryBranch)) match
          //   case Right(nodes) => nodes
          //   case Left(e) => boundary.break(result(e))

          // result = Some(AstNode2(
          //   Tag.effect_elimination,
          //   parser.currentSpan(),
          //   left,
          //   branches
          // ))
          boundary.break(result(parser.invalidTerm("effect elimination", "effect elimination not yet implemented")))
        }

      case lex.Tag.`!` =>
        // 错误抛出或消除
        parser.eatTokens(1)
        if (!parser.peek(lex.Tag.`{`)) { res = Some(AstNode1(Tag.error_throw, parser.currentSpan(), left)) }
        else {
          // 错误消除，需要解析分支
          // val branches = tryBlock(parser,
          //   Rule(Tag.catch_branch, tryCatchBranch),
          //   Rule(Tag.branch, tryBranch)
          // ) match
          //   case Right(nodes) => nodes
          //   case Left(e) => boundary.break(result(e))

          // result = Some(AstNode2(
          //   Tag.error_elimination,
          //   parser.currentSpan(),
          //   left,
          //   branches
          // ))
          boundary.break(result(parser.invalidTerm("error elimination", "error elimination not yet implemented")))
        }

      case lex.Tag.`?` =>
        // 可选值解包或消除
        parser.eatTokens(1)
        if (!parser.peek(lex.Tag.`{`)) { res = Some(AstNode1(Tag.option_unwrap, parser.currentSpan(), left)) }
        else {
          // 选项消除，需要解析块
          // val block = tryBlock(parser, Rule(Tag.statement, tryStatement, lex.Tag.`;`)) match
          //   case Right(nodes) => nodes
          //   case Left(e) => boundary.break(result(e))

          // result = Some(AstNode2(
          //   Tag.option_elimination,
          //   parser.currentSpan(),
          //   left,
          //   block
          // ))
          boundary.break(result(parser.invalidTerm("option elimination", "option elimination not yet implemented")))
        }

      case lex.Tag.k_match =>
        // 后置匹配
        parser.eatTokens(1)
        if (!parser.peek(lex.Tag.`{`)) boundary
          .break(result(parser.invalidTerm("match block", "expected a `{` to start a match block")))

        // 匹配块，需要解析分支
        // val branches = tryBlock(parser, Rule(Tag.branch, tryBranch)) match
        //   case Right(nodes) => nodes
        //   case Left(e) => boundary.break(result(e))

        // result = Some(AstNode2(
        //   Tag.post_match,
        //   parser.currentSpan(),
        //   left,
        //   branches
        // ))
        boundary.break(result(parser.invalidTerm("match block", "match block not yet implemented")))

      case lex.Tag.k_matches =>
        // 匹配模式
        parser.eatTokens(1)
        val pattern = tryPattern(parser, PatternOption(true)) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidTerm("pattern", "expected a pattern after `matches`")))

        res = Some(AstNode2(Tag.bool_matches, parser.currentSpan(), left, pattern))
      // boundary.break(
      //   result(
      //     parser.invalidTerm(
      //       "matches pattern",
      //       "matches pattern not yet implemented"
      //     )
      //   )
      // )

      case lex.Tag.id =>
        // 字面量扩展
        val currentToken = parser.currentToken()
        currentToken.tag match {
          case lex.Tag.str => tryId(parser) match {
              case Right(Some(id)) => res = Some(AstNode2(Tag.str_extension, parser.currentSpan(), left, id))
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(None))
            }

          case lex.Tag.int => tryId(parser) match {
              case Right(Some(id)) => res = Some(AstNode2(Tag.int_extension, parser.currentSpan(), left, id))
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(None))
            }

          case lex.Tag.char => tryId(parser) match {
              case Right(Some(id)) => res = Some(AstNode2(Tag.char_extension, parser.currentSpan(), left, id))
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(None))
            }

          case lex.Tag.real => tryId(parser) match {
              case Right(Some(id)) => res = Some(AstNode2(Tag.real_extension, parser.currentSpan(), left, id))
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(None))
            }

          case _ => boundary.break(result(None))
        }

      case _ => boundary.break(result(None))
    }

    result(res.get)
  }
// }

def tryPrefixExpr(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    tryLiteral(parser) match
      case Right(Some(value)) => boundary.break(result(value))
      case _ => ()

    val token = parser.peekToken()
    token.tag match {
      case lex.Tag.`(` => tryUnitOrParenthesisOrTuple(parser)
      case lex.Tag.`[` => tryList(parser)
      case lex.Tag.`{` => tryRecord(parser)
      case lex.Tag.`.` => trySymbol(parser)

      case lex.Tag.k_do => tryQualifiedBlock(parser, Tag.do_block, lex.Tag.k_do)
      case lex.Tag.k_async => tryQualifiedBlock(parser, Tag.async_block, lex.Tag.k_async)
      case lex.Tag.k_atomic => tryQualifiedBlock(parser, Tag.atomic_block, lex.Tag.k_atomic)
      case lex.Tag.k_unsafe => tryQualifiedBlock(parser, Tag.unsafe_block, lex.Tag.k_unsafe)
      case lex.Tag.k_comptime => tryQualifiedBlock(parser, Tag.comptime_block, lex.Tag.k_comptime)

      case lex.Tag.k_inline => tryPrefixTerm(parser, Tag.inline_def, lex.Tag.k_inline, tryExpr)
      case lex.Tag.k_dyn => tryPrefixTerm(parser, Tag.trait_object, lex.Tag.k_dyn, tryExpr)
      case lex.Tag.`*` => tryPrefixTerm(parser, Tag.pointer_type, lex.Tag.`*`, tryExpr)
      case lex.Tag.`?` => tryPrefixTerm(parser, Tag.optional, lex.Tag.`?`, tryExpr)

      case _ => result(None)
    }
  }
}

// tuple -> ( expr, expr* )
// parenthesis -> ( epxr )
// unit -> ()
def tryUnitOrParenthesisOrTuple(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`(`)) {
  boundary[ParseResult] {
    if (parser.eatToken(lex.Tag.`)`)) { boundary.break(result(AstNode0(Tag.unit, parser.currentSpan()))) }
    else {
      val expr = tryExpr(parser) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary
            .break(result(parser.invalidTerm("expression", "parsing a parenthesis expression or a tuple")))
      if (parser.eatToken(lex.Tag.`,`)) {
        val tail = tryMulti(parser, None, "parsing a tuple tail", lex.Tag.`,`, Rule(Tag.expr, tryExpr)) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))

        result(AstNodeN(Tag.tuple, parser.currentSpan(), expr :: tail))
      } else {
        if (!parser.eatToken(lex.Tag.`)`)) boundary
          .break(result(parser.invalidTerm("expression", "parsing a parenthesis expression or a tuple")))

        result(AstNode1(Tag.parenthesis, parser.currentSpan(), expr))
      }
    }
  }
}

// list -> [ expr* ]
def tryList(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`[`), true) {
  boundary[ParseResult] {
    val nodes = tryMulti(parser, Some(lex.Tag.`[`), "parsing a list", lex.Tag.`,`, Rule(Tag.expr, tryExpr)) match
      case Right(nodes) => nodes
      case Left(e) => boundary.break(result(e))

    result(AstNodeN(Tag.list, parser.currentSpan(), nodes))
  }
}

// record -> { property* }
def tryRecord(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`{`), true) {
  boundary[ParseResult] {
    val nodes =
      tryMulti(parser, Some(lex.Tag.`{`), "parsing a record", lex.Tag.`,`, Rule(Tag.property, tryProperty)) match
        case Right(nodes) => nodes
        case Left(e) => boundary.break(result(e))

    result(AstNodeN(Tag.record, parser.currentSpan(), nodes))
  }
}

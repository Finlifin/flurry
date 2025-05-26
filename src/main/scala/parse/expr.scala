package parse

import scala.util.boundary
import scala.util.chaining._
import scala.util.control.Breaks
import scala.annotation.switch

class ExprOption(val noRecordCall: Boolean = false, val precedence: Int = 0)
def tryExpr(parser: Parser): ParseResult = withCtx(parser)(tryPratt(parser, 0, ExprOption()))
def tryExpr(parser: Parser, opt: ExprOption): ParseResult = withCtx(parser)(tryPratt(parser, opt.precedence, opt))

// TODO: 待删除
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
    lex.Tag.`<:` -> OpInfo(40, Tag.subtype_with),
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

    // 90 级别保留用于常规前缀结合

    lex.Tag.`(` -> OpInfo(100, Tag.call),
    lex.Tag.`[` -> OpInfo(100, Tag.index_call),
    lex.Tag.`{` -> OpInfo(100, Tag.object_call),
    lex.Tag.`<` -> OpInfo(100, Tag.diamond_call),
    lex.Tag.`#` -> OpInfo(100, Tag.effect_elimination),
    lex.Tag.`!` -> OpInfo(100, Tag.error_elimination),
    lex.Tag.`?` -> OpInfo(100, Tag.option_elimination),
    lex.Tag.k_match -> OpInfo(100, Tag.post_match),
    lex.Tag.`.` -> OpInfo(110, Tag.select),
    lex.Tag.`'` -> OpInfo(110, Tag.image),

    // 字面量扩展
    lex.Tag.id -> OpInfo(120, Tag.id)
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
          case Left(ParseError.MeetRecordStart) | Left(ParseError.MeetPostIdSuccessor) => loop.break()
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
            currentLeft = token.tag match
              case lex.Tag.`_+_` => Ast.Add(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`_-_` => Ast.Sub(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`_*_` => Ast.Mul(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`_/_` => Ast.Div(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`_%_` => Ast.Mod(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`==` => Ast.BoolEq(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`!=` => Ast.BoolNotEq(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`>=` => Ast.BoolGtEq(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`_>_` => Ast.BoolGt(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`<=` => Ast.BoolLtEq(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`_<_` => Ast.BoolLt(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`==>` => Ast.BoolImplies(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.k_and => Ast.BoolAnd(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.k_or => Ast.BoolOr(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`:` => Ast.TypeWith(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`:-` => Ast.TraitBound(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`|` => Ast.Pipe(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`|>` => Ast.PipePrepend(currentLeft, right).withSpan(parser.currentSpan())
              case lex.Tag.`++` => Ast.AddAdd(currentLeft, right).withSpan(parser.currentSpan())
              case _ => throw new IllegalArgumentException(s"Unsupported operator: ${token.tag}")
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
def tryPostfixExpr(parser: Parser, tag: lex.Tag, left: Ast, opt: ExprOption): ParseResult =
  // withCtx(parser) {
  boundary {
    var res: Option[Ast] = None
    tag match {
      case lex.Tag.`(` =>
        // 函数调用
        val nodes = tryMulti(
          parser,
          Some(lex.Tag.`(`),
          "parsing a call",
          Rule("function argument", tryExpr),
          Rule("optional function argument", tryPropertyAssign)
          // Rule(Tag.expand_items, tryExpandItems)
        ) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))

        res = Some(Ast.Call(left, nodes.toMList).withSpan(parser.currentSpan()))

      case lex.Tag.`<` =>
        // 泛型调用（类似函数调用）
        val nodes = tryMulti(
          parser,
          Some(lex.Tag.`<`),
          "parsing a diamond call",
          Rule("diamond argument", tryExpr),
          Rule("optional diamond argument", tryPropertyAssign)
        ) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))

        res = Some(Ast.DiamondCall(left, nodes.toMList).withSpan(parser.currentSpan()))

      case lex.Tag.`{` =>
        // 记录调用（类似函数调用）
        if (opt.noRecordCall) boundary.break(result(ParseError.MeetRecordStart))

        val nodes = tryMulti(
          parser,
          Some(lex.Tag.`{`),
          "parsing a record call",
          Rule("optional record argument", tryProperty),
          Rule("record argument", tryExpr)
        ) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))

        res = Some(Ast.ObjectCall(left, nodes.toMList).withSpan(parser.currentSpan()))

      case lex.Tag.`[` =>
        // 索引调用
        parser.eatTokens(1)
        val indexExpr = tryExpr(parser) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidTerm("expression", "parsing an indexing expression")))

        if (!parser.eatToken(lex.Tag.`]`)) boundary
          .break(result(ParseError.UnexpectedToken(lex.Tag.`]`, parser.peekToken(), "closing a indexing expression")))

        res = Some(Ast.IndexCall(left, indexExpr).withSpan(parser.currentSpan()))

      case lex.Tag.`.` =>
        // 点操作符相关
        parser.eatTokens(1)
        val next = parser.peekToken()

        next.tag match
          case lex.Tag.`*` =>
            // 解引用
            parser.eatTokens(1)
            res = Some(Ast.Deref(left).withSpan(parser.currentSpan()))

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

            res = Some(Ast.HandlerApply(left, handler).withSpan(parser.currentSpan()))

          case lex.Tag.k_ref =>
            // 引用
            parser.eatTokens(1)
            res = Some(Ast.Refer(left).withSpan(parser.currentSpan()))

          case lex.Tag.k_await =>
            // 等待
            parser.eatTokens(1)
            res = Some(Ast.Await(left).withSpan(parser.currentSpan()))

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

            res = Some(Ast.AsDyn(left, trait_expr).withSpan(parser.currentSpan()))

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

            res = Some(Ast.TypeCast(left, typeExpr).withSpan(parser.currentSpan()))

          case lex.Tag.`.` =>
            // 区间表达式
            if (parser.peek(lex.Tag.`.`, lex.Tag.`=`)) {
              parser.eatTokens(2)
              val end = tryExpr(parser, ExprOption(true)) match
                case Right(Some(node)) => node
                case Left(e) => boundary.break(result(e))
                case _ => boundary.break(result(parser.invalidTerm("expression", "parsing a range expression")))

              res = Some(Ast.RangeFromToInclusive(left, end).withSpan(parser.currentSpan()))
            } else {
              parser.eatTokens(1)
              tryExpr(parser, ExprOption(true)) match
                case Right(Some(end)) => res = Some(Ast.RangeFromTo(left, end).withSpan(parser.currentSpan()))
                case Right(None) => res = Some(Ast.RangeFrom(left).withSpan(parser.currentSpan()))
                case Left(e) => boundary.break(result(e))
            }

          case _ =>
            // 字段选择
            tryId(parser) match
              case Right(Some(id)) => res = Some(Ast.Select(left, id).withSpan(parser.currentSpan()))
              case Left(e) => boundary.break(result(e))
              case _ => boundary.break(result(parser.invalidTerm("identifier", "parsing symbol selection expression")))

      case lex.Tag.`'` => tryId(parser) match
          case Right(Some(id)) => res = Some(Ast.Image(left, id).withSpan(parser.currentSpan()))
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidTerm("identifier", "parsing an image selection expression")))

      case lex.Tag.`#` =>
        // 效果发射或消除
        parser.eatTokens(1)
        if (!parser.peek(lex.Tag.`{`)) { res = Some(Ast.EffectPropagation(left)) }
        else
          val branches = tryMulti(
            parser,
            Some(lex.Tag.`{`),
            "parsing an effect handler",
            Rule("effect branch", tryPatternBranch)
          ) match
            case Right(nodes) => nodes
            case Left(e) => boundary.break(result(e))

          res = Some(Ast.EffectElimination(left, branches.toMList).withSpan(parser.currentSpan()))

      case lex.Tag.`!` =>
        // 错误抛出或消除
        parser.eatTokens(1)
        if (!parser.peek(lex.Tag.`{`)) { res = Some(Ast.ErrorPropagation(left)) }
        else
          val branches = tryMulti(
            parser,
            Some(lex.Tag.`{`),
            "parsing an error handler",
            Rule("error handler catch branch", tryCatchBranch),
            Rule("error handler branch", tryPatternBranch)
          ) match
            case Right(nodes) => nodes
            case Left(e) => boundary.break(result(e))

          res = Some(Ast.ErrorElimination(left, branches.toMList).withSpan(parser.currentSpan()))

      case lex.Tag.`?` =>
        // 可选值解包或消除
        parser.eatTokens(1)
        if (!parser.peek(lex.Tag.`{`)) { res = Some(Ast.OptionPropagation(left)) }
        else
          // 选项消除，需要解析块
          val block = tryBlock(parser) match
            case Right(Some(node)) => node
            case Left(e) => boundary.break(result(e))
            case _ => boundary.break(result(parser.invalidTerm("block", "parsing an option elimination block")))

          res = Some(Ast.OptionElimination(left, block).withSpan(parser.currentSpan()))

      case lex.Tag.k_match =>
        // 后置匹配
        parser.eatTokens(1)
        if (!parser.peek(lex.Tag.`{`)) boundary
          .break(result(parser.invalidTerm("match block", "expected a `{` to start a match block")))

        val branches =
          tryMulti(parser, Some(lex.Tag.`{`), "parsing a match block", Rule("match branch", tryPatternBranch)) match
            case Right(nodes) => nodes
            case Left(e) => boundary.break(result(e))

        res = Some(Ast.Match(left, branches.toMList).withSpan(parser.currentSpan()))

      case lex.Tag.k_matches =>
        // 匹配模式
        parser.eatTokens(1)
        val pattern = tryPattern(parser, PatternOption(true)) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidTerm("pattern", "expected a pattern after `matches`")))

        res = Some(Ast.BoolMatches(left, pattern).withSpan(parser.currentSpan()))

      case lex.Tag.id =>
        // 字面量扩展
        val currentToken = parser.currentToken()
        res = currentToken.tag match {
          case lex.Tag.str =>
            Some(Ast.StrExtension(parser.srcContentT(parser.nextToken()), left).withSpan(parser.currentSpan()))
          case lex.Tag.int =>
            Some(Ast.IntExtension(parser.srcContentT(parser.nextToken()), left).withSpan(parser.currentSpan()))
          case lex.Tag.char =>
            Some(Ast.CharExtension(parser.srcContentT(parser.nextToken()), left).withSpan(parser.currentSpan()))
          case lex.Tag.real =>
            Some(Ast.RealExtension(parser.srcContentT(parser.nextToken()), left).withSpan(parser.currentSpan()))
          case _ => boundary.break(result(ParseError.MeetPostIdSuccessor))
        }

      case _ => boundary.break(result(None))
    }

    result(res.get)
  }
// }

def tryPrefixExpr(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    tryAtom(parser) match
      case Right(Some(value)) => boundary.break(result(value))
      case _ => ()

    val token = parser.peekToken()
    token.tag match {
      case lex.Tag.`(` => tryUnitOrParenthesisOrTuple(parser)
      case lex.Tag.`[` => tryList(parser)
      case lex.Tag.`{` => tryObject(parser)
      case lex.Tag.`.` => trySymbol(parser)
      case lex.Tag.`|` => tryLambda(parser)
      case lex.Tag.k_forall => tryForallType(parser)
      case lex.Tag.`#` => tryEffectQualifiedType(parser)
      case lex.Tag.`!` => tryErrorQualifiedType(parser)
      case lex.Tag.k_not =>
        tryPrefixExpr(parser, lex.Tag.k_not, p => tryExpr(p, ExprOption(precedence = 90, noRecordCall = true)))
      case lex.Tag.k_dyn =>
        tryPrefixExpr(parser, lex.Tag.k_dyn, p => tryExpr(p, ExprOption(precedence = 90, noRecordCall = true)))
      case lex.Tag.`*` =>
        tryPrefixExpr(parser, lex.Tag.`*`, p => tryExpr(p, ExprOption(precedence = 90, noRecordCall = true)))
      case lex.Tag.`?` =>
        tryPrefixExpr(parser, lex.Tag.`?`, p => tryExpr(p, ExprOption(precedence = 90, noRecordCall = true)))

      case _ => result(None)
    }
  }
}

private def tryPrefixExpr(parser: Parser, prefix: lex.Tag, followRule: Parser => ParseResult): ParseResult =
  withCtx(parser, Some(prefix)) {
    followRule(parser) match
      case Right(Some(node)) => prefix match
          case lex.Tag.k_not => result(Ast.BoolNot(node))
          case lex.Tag.k_dyn => result(Ast.TraitObjectType(node))
          case lex.Tag.`?` => result(Ast.OptionalType(node))
          case lex.Tag.`*` => result(Ast.PointerType(node))
          case _ => result(None)
      case _ @res => res
  }

// tuple -> ( expr, expr* )
// parenthesis -> ( epxr )
// unit -> ()
def tryUnitOrParenthesisOrTuple(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`(`)) {
  boundary[ParseResult] {
    if (parser.eatToken(lex.Tag.`)`)) { boundary.break(result(Ast.Unit.withSpan(parser.currentSpan()))) }
    else {
      val expr = tryExpr(parser) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary
            .break(result(parser.invalidTerm("expression", "parsing a parenthesis expression or a tuple")))
      if (parser.eatToken(lex.Tag.`,`)) {

        val tail = tryMulti(parser, None, "parsing a tuple tail", Rule("tuple element", tryExpr)) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))
        if (!parser.eatToken(lex.Tag.`)`)) boundary
          .break(result(ParseError.UnexpectedToken(lex.Tag.`)`, parser.peekToken(), "closing a tuple expression")))
        result(Ast.Tuple((expr :: tail).toMList).withSpan(parser.currentSpan()))
      } else {
        if (!parser.eatToken(lex.Tag.`)`)) boundary
          .break(result(parser.invalidTerm("expression", "parsing a parenthesis expression or a tuple")))

        result(expr)
      }
    }
  }
}

// list -> [ expr* ]
def tryList(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`[`), true) {
  boundary[ParseResult] {
    val nodes = tryMulti(parser, Some(lex.Tag.`[`), "parsing a list", Rule("list element", tryExpr)) match
      case Right(nodes) => nodes
      case Left(err) => boundary.break(result(err))

    result(Ast.List(nodes.toMList).withSpan(parser.currentSpan()))
  }
}

// object -> { property* }
def tryObject(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`{`), true) {
  boundary[ParseResult] {
    val nodes = tryMulti(
      parser,
      Some(lex.Tag.`{`),
      "parsing a record",
      Rule("property", tryProperty),
      Rule("child expr", tryExpr)
    ) match
      case Right(nodes) => nodes
      case Left(err) => boundary.break(result(err))

    result(Ast.Object(nodes.toMList).withSpan(parser.currentSpan()))
  }
}

// forall<id | param> expr(precedence = 90)
def tryForallType(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_forall)) {
  boundary[ParseResult] {
    if (!parser.peek(lex.Tag.`<`)) {
      boundary.break(result(parser.invalidTerm("forall type", "expected a `<` to start a forall type")))
    }

    val params = tryMulti(
      parser,
      Some(lex.Tag.`<`),
      "parsing a forall type declaration list",
      Rule("type parameter", tryParam)
    ) match
      case Right(nodes) => nodes
      case Left(err) => boundary.break(result(err))

    val expr = tryExpr(parser) match
      case Right(Some(node)) => node
      case Left(err) => boundary.break(result(err))
      case _ => boundary.break(result(parser.invalidTerm("expression", "parsing a forall type")))

    result(Ast.ForallType(params.toMList, expr).withSpan(parser.currentSpan()))
  }
}

// effect_qualified_type -> #expr expr
def tryEffectQualifiedType(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`#`)) {
  boundary[ParseResult] {
    val effectList = tryExpr(parser, ExprOption(precedence = 90)) match
      case Right(Some(node)) => node
      case Left(err) => boundary.break(result(err))
      case _ => boundary
          .break(result(parser.invalidTerm("expression", "parsing effect list of an effect qualified type")))

    val expr = tryExpr(parser, ExprOption(precedence = 90)) match
      case Right(Some(node)) => node
      case Left(err) => boundary.break(result(err))
      case _ => boundary.break(result(parser.invalidTerm("expression", "parsing an effect qualified type")))

    result(Ast.EffectQualifiedType(effectList, expr).withSpan(parser.currentSpan()))
  }
}

// error_qualified_type -> !expr expr
def tryErrorQualifiedType(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`!`)) {
  boundary[ParseResult] {
    val errorList = tryExpr(parser, ExprOption(precedence = 90)) match
      case Right(Some(node)) => node
      case Left(err) => boundary.break(result(err))
      case _ => boundary
          .break(result(parser.invalidTerm("expression", "parsing error list of an error qualified type")))

    val expr = tryExpr(parser, ExprOption(precedence = 90)) match
      case Right(Some(node)) => node
      case Left(err) => boundary.break(result(err))
      case _ => boundary.break(result(parser.invalidTerm("expression", "parsing an error qualified type")))

    result(Ast.ErrorQualifiedType(errorList, expr).withSpan(parser.currentSpan()))
  }
}

// // lambda -> |(id | param)*| return_type? block|expr
// def tryLambda(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`|`), true) {
//   boundary[ParseResult] {
//     val params = tryMulti(
//       parser,
//       Some(lex.Tag.`|`),
//       "parsing a lambda parameter list",
//       Rule(Tag.param, tryParam),
//       Rule(Tag.id, tryId)
//     ) match
//       case Right(nodes) => AstNodeN(Tag.params, parser.currentSpan(), nodes)
//       case Left(e) => boundary.break(result(e))

//     val returnType =
//       if (parser.peek(lex.Tag.`->`)) {
//         parser.eatTokens(1)
//         tryExpr(parser) match
//           case Right(Some(node)) => node
//           case Left(e) => boundary.break(result(e))
//           case _ => boundary.break(result(parser.invalidTerm("expression", "parsing a lambda return type")))
//       } else { null }

//     val block_or_expr = tryBlock(parser) match
//       case Right(Some(node)) => node
//       case Left(e) => boundary.break(result(e))
//       case _ => tryExpr(parser) match
//           case Right(Some(node)) => node
//           case Left(e) => boundary.break(result(e))
//           case _ => boundary.break(result(parser.invalidTerm("expression", "parsing a lambda block or expression")))

//     result(AstNode3(Tag.lambda, parser.currentSpan(), params, returnType, block_or_expr))
//   }
// }
def tryLambda(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`|`), true) {
  boundary[ParseResult] {
    val params = tryMulti(parser, Some(lex.Tag.`|`), "parsing a lambda parameter list", Rule("parameter", tryId)) match
      case Right(nodes) => nodes
      case Left(err) => boundary.break(result(err))

    var returnType: Ast = null
    if (parser.peek(lex.Tag.`->`)) {
      parser.eatTokens(1)
      returnType = tryExpr(parser) match
        case Right(Some(node)) => node
        case Left(err) => boundary.break(result(err))
        case _ => boundary.break(result(parser.invalidTerm("expression", "parsing a lambda return type")))
    }

    // 尝试解析块或表达式
    val blockOrExpr = tryExpr(parser) match
      case Right(Some(node)) => node
      case Left(err) => boundary.break(result(err))
      case _ => boundary.break(result(parser.invalidTerm("expression", "parsing a lambda body")))

    // 创建 Lambda AST 节点
    // 注意：这里我们创建一个 lambda，根据 Ast 的情况可能需要调整
    if (returnType == null) {
      result(
        Ast.Lambda(params.map(p => p.asInstanceOf[Ast.Id].name).toMList, blockOrExpr).withSpan(parser.currentSpan())
      )
    } else {
      result(
        Ast.LambdaWithType(params.map(p => p.asInstanceOf[Ast.Id].name).toMList, returnType, blockOrExpr)
          .withSpan(parser.currentSpan())
      )
    }
  }
}

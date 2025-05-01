package parse

import scala.util.boundary
import scala.util.chaining._
import scala.util.control.Breaks
import java.util.regex.Pattern

case class PatternOption(noRecordCall: Boolean = false, precedence: Int = 0)

def tryPattern(parser: Parser): ParseResult = withCtx(parser)(tryPattern(parser, PatternOption()))

def tryPattern(parser: Parser, opt: PatternOption): ParseResult = withCtx(parser) {
  parser.enter()
  try tryPatternPratt(parser, opt.precedence, opt)
  finally parser.exit()
}

/** Operator table mapping token tags to operator information */
object PatternOpTable {
  private val DEFAULT_OP_INFO = OpInfo(-1, Tag.invalid)

  val table: Map[lex.Tag, OpInfo] = Map(
    lex.Tag.k_if -> OpInfo(10, Tag.pattern_if_guard),
    lex.Tag.k_and -> OpInfo(10, Tag.pattern_and_is),
    lex.Tag.k_as -> OpInfo(20, Tag.pattern_as_bind),
    lex.Tag.k_or -> OpInfo(30, Tag.pattern_or),
    lex.Tag.`?` -> OpInfo(40, Tag.pattern_option_some),
    lex.Tag.`!` -> OpInfo(40, Tag.pattern_error_ok),
    lex.Tag.`(` -> OpInfo(80, Tag.pattern_call),
    lex.Tag.`{` -> OpInfo(80, Tag.pattern_object_call),
    lex.Tag.`<` -> OpInfo(80, Tag.pattern_diamond_call),
    lex.Tag.`.` -> OpInfo(90, Tag.select)
  )

  /** Get operator information, returning default if not found */
  def getOpInfo(tag: lex.Tag): OpInfo = table.getOrElse(tag, DEFAULT_OP_INFO)
}

/** Pratt parsing for patterns
  * @param parser
  *   Parser instance
  * @param minPrec
  *   Minimum precedence
  * @param opt
  *   Pattern options
  * @return
  *   Parse result
  */
def tryPatternPratt(parser: Parser, minPrec: Int, opt: PatternOption): ParseResult = withCtx(parser) {
  boundary {
    // Try to parse prefix pattern as left operand
    val left = tryPrefixPattern(parser) match
      case Right(Some(node)) => node
      case Right(None) => boundary.break(result(None))
      case Left(error) => boundary.break(result(error))

    // Process operators and right operands
    var currentLeft = left
    val loop = new Breaks
    loop.breakable {
      while (true) {
        val token = parser.peekToken()
        val opInfo = PatternOpTable.getOpInfo(token.tag)

        // Exit loop if operator is invalid or has too low precedence
        if (opInfo.tag == Tag.invalid || opInfo.prec < minPrec) { loop.break }

        // Try postfix pattern
        tryPostfixPattern(parser, token.tag, currentLeft, opInfo.prec + 1, opt) match {
          case Right(Some(node)) => currentLeft = node
          // Continue to next iteration
          case Left(ParseError.MeetRecordStart) => loop.break()
          case Left(e) => boundary.break(Left(e))
          case _ =>
            // Consume operator token
            parser.eatTokens(1)

            // Parse right operand with higher precedence
            val right = tryPatternPratt(parser, opInfo.prec + 1, opt) match
              case Right(Some(node)) => node
              case _ => boundary.break(result(parser.invalidPattern("missing right operand")))

            // Create binary operator node
            currentLeft = AstNode2(opInfo.tag, parser.currentSpan(), currentLeft, right)
        }
      }
    }

    // Return final constructed pattern
    result(currentLeft)
  }
}

/** Try to parse a prefix pattern
  * @param parser
  *   Parser instance
  * @return
  *   Parse result
  */
def tryPrefixPattern(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (parser.peek(lex.Tag.int, lex.Tag.id) && parser.srcContentT(parser.getToken(parser.cursor + 1)) == "0") {
      if (parser.srcContentT(parser.getToken(parser.cursor + 2)).charAt(0) == 'x') {
        boundary.break(tryBitVecPattern(parser, 'x'))
      } else if (parser.srcContentT(parser.getToken(parser.cursor + 2)).charAt(0) == 'o') {
        boundary.break(tryBitVecPattern(parser, 'o'))
      } else if (parser.srcContentT(parser.getToken(parser.cursor + 2)).charAt(0) == 'b') {
        boundary.break(tryBitVecPattern(parser, 'b'))
      }
    }

    // Try literal
    tryLiteral(parser) match
      case Right(Some(value)) => boundary.break(result(value))
      case _ => ()

    val token = parser.peekToken()
    token.tag match {
      case lex.Tag.`.` =>
        // Try range-to or symbol
        tryRangeTo(parser) match
          case Right(Some(node)) => boundary.break(result(node))
          case _ => ()

        trySymbol(parser) match
          case Right(Some(node)) => boundary.break(result(node))
          case _ => ()

        boundary.break(result(None))
      case lex.Tag.`[` => tryListPattern(parser)
      case lex.Tag.`(` => tryTuplePattern(parser)
      case lex.Tag.`{` => tryRecordPattern(parser)
      case lex.Tag.`<` => tryPatternFromExpr(parser)

      case lex.Tag.k_async => tryPrefixTerm(
          parser,
          Tag.pattern_async,
          lex.Tag.k_async,
          parser => tryPattern(parser, PatternOption(precedence = 0))
        )
      case lex.Tag.k_not => tryPrefixTerm(
          parser,
          Tag.pattern_not,
          lex.Tag.k_not,
          parser => tryPattern(parser, PatternOption(precedence = 0))
        )
      case lex.Tag.`'` => tryPrefixTerm(parser, Tag.pattern_type_bind, lex.Tag.`'`, tryId)

      case _ => result(None)
    }
  }
}

/** Try to parse a postfix pattern
  * @param parser
  *   Parser instance
  * @param tag
  *   Current token tag
  * @param left
  *   Left pattern node
  * @param opt
  *   Pattern options
  * @return
  *   Parse result
  */
def tryPostfixPattern(parser: Parser, tag: lex.Tag, left: AstNode, minPrec: Int, opt: PatternOption): ParseResult =
//   withCtx(parser) {
  boundary {
    var res: Option[AstNode] = None

    tag match {
      case lex.Tag.`(` =>
        // Pattern call
        val nodes = tryMulti(parser, Some(lex.Tag.`(`), "parsing a pattern call", Rule(Tag.pattern, tryPattern)) match
          case Right(nodes) => nodes
          case Left(e) => boundary.break(result(e))

        res = Some(AstNodeL(Tag.pattern_call, parser.currentSpan(), left, nodes))

      case lex.Tag.`<` =>
        // Diamond pattern call
        val nodes =
          tryMulti(parser, Some(lex.Tag.`<`), "parsing a diamond pattern call", Rule(Tag.pattern, tryPattern)) match
            case Right(nodes) => nodes
            case Left(e) => boundary.break(result(e))

        res = Some(AstNodeL(Tag.pattern_diamond_call, parser.currentSpan(), left, nodes))

      case lex.Tag.`.` =>
        // Range patterns
        parser.eatTokens(1)
        if (parser.peek(lex.Tag.`.`, lex.Tag.`=`)) {
          parser.eatTokens(2)
          val end = tryPattern(parser, PatternOption(noRecordCall = true)) match
            case Right(Some(node)) => node
            case Left(e) => boundary.break(result(e))
            case _ => boundary.break(result(parser.invalidPattern("expected an expression after `..=`")))

          res = Some(AstNode2(Tag.pattern_range_from_to_inclusive, parser.currentSpan(), left, end))
        } else {
          parser.eatTokens(1)
          tryPattern(parser, PatternOption(noRecordCall = true)) match {
            case Right(Some(end)) => res = Some(AstNode2(Tag.pattern_range_from_to, parser.currentSpan(), left, end))
            case Right(None) => res = Some(AstNode1(Tag.pattern_range_from, parser.currentSpan(), left))
            case Left(e) => boundary.break(result(e))
          }
        }

      case lex.Tag.`{` =>
        // Record call pattern
        if (opt.noRecordCall) boundary.break(Left(ParseError.MeetRecordStart))

        tryMulti(
          parser,
          Some(lex.Tag.`{`),
          "parsing a record call pattern",
          Rule(Tag.pattern, tryPropertyPattern),
          Rule(Tag.id, tryId)
        ) match
          case Right(nodes) => res = Some(AstNodeL(Tag.pattern_object_call, parser.currentSpan(), left, nodes))
          case Left(e) =>
            // parser.fallback();
            boundary.break(Left(ParseError.MeetRecordStart))

      case lex.Tag.k_as =>
        // As binding
        parser.eatTokens(1)
        val id = tryId(parser) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidPattern("expected an identifier after `as`")))

        res = Some(AstNode2(Tag.pattern_as_bind, parser.currentSpan(), left, id))

      case lex.Tag.k_if =>
        // If guard
        parser.eatTokens(1)
        val guard = tryExpr(parser, ExprOption(true)) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidPattern("expected an expression after `if`")))

        res = Some(AstNode2(Tag.pattern_if_guard, parser.currentSpan(), left, guard))

      case lex.Tag.k_and =>
        // And-is pattern
        parser.eatTokens(1)
        val expr = tryExpr(parser) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidPattern("expected an expression after `and`")))

        if (!parser.eatToken(lex.Tag.k_is)) boundary.break(result(parser.invalidPattern("missing 'is' after 'and'")))

        val pattern = tryPatternPratt(parser, minPrec, opt) match
          case Right(Some(node)) => node
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidPattern("expected a pattern after `is`")))

        res = Some(AstNode3(Tag.pattern_and_is, parser.currentSpan(), left, expr, pattern))

      case lex.Tag.`?` =>
        // Option some pattern
        parser.eatTokens(1)
        res = Some(AstNode1(Tag.pattern_option_some, parser.currentSpan(), left))

      case lex.Tag.`!` =>
        // Option some pattern
        parser.eatTokens(1)
        res = Some(AstNode1(Tag.pattern_error_ok, parser.currentSpan(), left))
      case _ => boundary.break(result(None))
    }

    result(res.get)
  }
//   }

/** Try to parse a pattern from an expression
  * @param parser
  *   Parser instance
  * @return
  *   Parse result
  */
def tryPatternFromExpr(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`<`)) {
  boundary {
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidPattern("expected an expression after `<`")))

    if (!parser.eatToken(lex.Tag.`>`)) boundary.break(result(parser.invalidPattern("expected `>` after expression")))

    result(AstNode1(Tag.pattern_from_expr, parser.currentSpan(), expr))
  }
}

/** Try to parse a range-to pattern (..pattern or ..=pattern)
  * @param parser
  *   Parser instance
  * @return
  *   Parse result
  */
def tryRangeTo(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (parser.peek(lex.Tag.`.`, lex.Tag.`.`, lex.Tag.`=`)) {
      parser.eatTokens(3)
      val end = tryPattern(parser, PatternOption(noRecordCall = true)) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidPattern("expected a pattern after `..=`")))

      boundary.break(result(AstNode1(Tag.pattern_range_to_inclusive, parser.currentSpan(), end)))
    } else if (parser.peek(lex.Tag.`.`, lex.Tag.`.`)) {
      parser.eatTokens(2)
      val end = tryPattern(parser, PatternOption(noRecordCall = true)) match
        case Right(Some(node)) => node
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidPattern("expected a pattern after `..`")))

      boundary.break(result(AstNode1(Tag.pattern_range_to, parser.currentSpan(), end)))
    }

    result(None)
  }
}

/** Try to parse a property pattern (id: pattern)
  * @param parser
  *   Parser instance
  * @return
  *   Parse result
  */
def tryPropertyPattern(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`:`)) { boundary.break(result(None)) }

    val id = tryId(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidPattern("expected an identifier")))

    parser.eatTokens(1) // Consume the colon

    val pattern = tryPattern(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidPattern("expected a pattern after `:`")))

    result(AstNode2(Tag.property_pattern, parser.currentSpan(), id, pattern))
  }
}

/** Try to parse a record pattern {props}
  * @param parser
  *   Parser instance
  * @return
  *   Parse result
  */
def tryRecordPattern(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`{`), true) {
  boundary {
    val nodes = tryMulti(
      parser,
      Some(lex.Tag.`{`),
      "parsing a record pattern",
      Rule(Tag.property_pattern, tryPropertyPattern),
      Rule(Tag.id, tryId)
    ) match
      case Right(nodes) => nodes
      case Left(e) => boundary.break(result(e))

    result(AstNodeN(Tag.pattern_record, parser.currentSpan(), nodes))
  }
}

/** Try to parse a list pattern [items]
  * @param parser
  *   Parser instance
  * @return
  *   Parse result
  */
def tryListPattern(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`[`), true) {
  boundary {
    val nodes = tryMulti(parser, Some(lex.Tag.`[`), "parsing a list pattern", Rule(Tag.pattern, tryPattern)) match
      case Right(nodes) => nodes
      case Left(e) => boundary.break(result(e))

    result(AstNodeN(Tag.pattern_list, parser.currentSpan(), nodes))
  }
}

/** Try to parse a tuple pattern (items)
  * @param parser
  *   Parser instance
  * @return
  *   Parse result
  */
def tryTuplePattern(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`(`), true) {
  boundary {
    val nodes = tryMulti(parser, Some(lex.Tag.`(`), "parsing a tuple pattern", Rule(Tag.pattern, tryPattern)) match
      case Right(nodes) => nodes
      case Left(e) => boundary.break(result(e))

    result(AstNodeN(Tag.pattern_tuple, parser.currentSpan(), nodes))
  }
}

// id: pattern
def tryIdExprPair(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.id), true) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`:`)) { boundary.break(result(None)) }

    val id = tryId(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidPattern("expected an identifier")))

    if (!parser.eatToken(lex.Tag.`:`)) {
      boundary.break(result(parser.invalidPattern("expected `:` after identifier")))
    }

    val pattern = tryExpr(parser) match
      case Right(Some(node)) => node
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidPattern("expected a pattern")))

    result(AstNode2(Tag.pair, parser.currentSpan(), id, pattern))
  }
}

def tryIdExprPairOrExpr(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`(`)) {
  boundary {
    val node = tryIdExprPair(parser) match
      case Right(Some(node)) => result(node)
      case Left(e) => boundary.break(result(e))
      case _ =>
        // parser.fallback();
        tryExpr(parser) match
          case Right(Some(node)) => result(node)
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(None))

    if (!parser.eatToken(lex.Tag.`)`)) { boundary.break(result(parser.invalidPattern("expected `)` after pattern"))) }
    node
  }
}

def tryInt(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.int), true)(tryLiteral(parser))

// pattern_bitvec_0x -> 0x (int | id | ( id : expr | expr ))*
def tryBitVecPattern(parser: Parser, kind: Char): ParseResult = withCtx(parser) {
  boundary {
    val tag = kind match
      case 'x' => Tag.pattern_bitvec_0x
      case 'o' => Tag.pattern_bitvec_0o
      case 'b' => Tag.pattern_bitvec_0b
      case _ => throw new IllegalArgumentException("Invalid kind for bit vector pattern")
    if (
      parser.peek(lex.Tag.int, lex.Tag.id) && parser.srcContentT(parser.getToken(parser.cursor + 1)) == "0" &&
      parser.srcContentT(parser.getToken(parser.cursor + 2)).charAt(0) == kind
    ) {
      parser.eatTokens(1)

      val nodes = tryMulti(
        parser,
        None,
        s"parsing a 0$kind bit vector pattern",
        Rule(Tag.int, tryInt, lex.Tag.invalid),
        Rule(Tag.id, tryId, lex.Tag.invalid),
        Rule(Tag.pair, tryIdExprPairOrExpr, lex.Tag.invalid)
      ) match
        case Right(nodes) => nodes
        case Left(e) => boundary.break(result(e))
      result(AstNodeN(tag, parser.currentSpan(), nodes))
    } else { result(None) }
  }
}

/** Extension for the Parser class to add pattern-specific functionality */
extension (parser: Parser) {
//   def tryPattern(): ParseResult = tryPattern(parser)
//   def tryPattern(opt: PatternOption): ParseResult = tryPattern(parser, opt)

  def invalidPattern(message: String): ParseError = {
    val token = parser.peekToken()
    ParseError.InvalidTerm("pattern", token, message)
  }
}

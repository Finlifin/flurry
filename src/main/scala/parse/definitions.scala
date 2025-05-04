package parse

import scala.util.boundary
import scala.util.control.Breaks

// param_optional -> .id: expr = expr
// param_typed -> id: expr
// param_trait_bound -> id: expr :- expr
// param_id -> id
// param_optional_id -> .id
// param_self -> self
// param_self_ref -> *self
// param_itself -> itself
// param_itself_ref -> *itself
// param_rest_bind -> ... id: expr
def tryParam(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (parser.eatToken(lex.Tag.k_implicit)) {
      val id = tryId(parser) match
        case Right(Some(id)) => id
        case _ => boundary.break(result(None))

      if (!parser.eatToken(lex.Tag.`:`)) boundary
        .break(result(parser.invalidTerm(":", "expected ':' after parameter name")))

      val typ = tryExpr(parser) match
        case Right(Some(t)) => t
        case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

      boundary.break(result(AstNode2(Tag.param_implicit, parser.currentSpan(), id, typ)))
    }

    if (parser.eatToken(lex.Tag.k_comptime)) {
      tryParam(parser) match
        case Right(Some(param)) => boundary.break(result(AstNode1(Tag.comptime_def, parser.currentSpan(), param)))
        case _ => boundary.break(result(parser.invalidTerm("parameter", "expected a parameter after 'comptime'")))
    }

    if (parser.peek(lex.Tag.`*`, lex.Tag.k_self)) {
      parser.eatTokens(2)
      boundary.break(result(AstNode0(Tag.param_self_ref, parser.currentSpan())))
    }

    if (parser.peek(lex.Tag.`*`, lex.Tag.k_itself)) {
      parser.eatTokens(2)
      boundary.break(result(AstNode0(Tag.param_itself_ref, parser.currentSpan())))
    }

    if (parser.eatToken(lex.Tag.k_self)) boundary.break(result(AstNode0(Tag.param_self, parser.currentSpan())))

    if (parser.eatToken(lex.Tag.k_itself)) boundary.break(result(AstNode0(Tag.param_itself, parser.currentSpan())))

    // ... id: expr (rest parameter)
    if (parser.peek(lex.Tag.`.`, lex.Tag.`.`, lex.Tag.`.`, lex.Tag.id)) {
      parser.eatTokens(3)
      val id = tryId(parser) match
        case Right(Some(id)) => id
        case _ => boundary
            .break(result(ParseError.UnexpectedToken(lex.Tag.id, parser.peekToken(), "parsing rest parameter")))

      if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(
        ParseError.UnexpectedToken(lex.Tag.`:`, parser.peekToken(), "varargs must be specified with a type")
      ))

      val typ = tryExpr(parser) match
        case Right(Some(t)) => t
        case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

      boundary.break(result(AstNode2(Tag.param_rest_bind, parser.currentSpan(), id, typ)))
    }

    // .id: expr (optional parameter)
    if (parser.peek(lex.Tag.`.`, lex.Tag.id)) {
      parser.eatTokens(1)
      val id = tryId(parser) match
        case Right(Some(id)) => id
        case _ => boundary.break(result(parser.invalidTerm("identifier", "parsing an optional parameter")))

      if (!parser.eatToken(lex.Tag.`:`)) boundary
        .break(result(AstNode1(Tag.param_optional_id, parser.currentSpan(), id)))

      val typ = tryExpr(parser) match
        case Right(Some(t)) => t
        case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

      boundary.break(result(AstNode2(Tag.param_optional, parser.currentSpan(), id, typ)))
    }

    // Regular parameter
    if (parser.peek(lex.Tag.id)) {
      val id = tryId(parser) match
        case Right(Some(id)) => id
        case _ => boundary.break(result(None))

      // Parameter with trait bound: id:- trait
      if (parser.eatToken(lex.Tag.`:-`)) {
        val trait_ = tryExpr(parser) match
          case Right(Some(t)) => t
          case _ => boundary.break(result(parser.invalidTerm("trait", "expected a trait after ':-'")))

        boundary.break(result(AstNode2(Tag.param_trait_bound, parser.currentSpan(), id, trait_)))
      }

      // Regular typed parameter: id: type
      if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(AstNode1(Tag.param_id, parser.currentSpan(), id)))

      val typ = tryExpr(parser) match
        case Right(Some(t)) => t
        case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

      boundary.break(result(AstNode2(Tag.param_typed, parser.currentSpan(), id, typ)))
    }

    result(None)
  }
}

// Try to parse a where clause
def tryClause(parser: Parser): ParseResult = withCtx(parser) {
  val token = parser.peekToken()
  token.tag match {
    case lex.Tag.id => tryClauseDecl(parser)
    case lex.Tag.`.` => tryClauseOptionalDecl(parser)
    case lex.Tag.k_requires => tryRequiresClause(parser)
    case lex.Tag.k_ensures => tryEnsuresClause(parser)
    case lex.Tag.k_decreases => tryDecreasesClause(parser)
    case _ => result(None)
  }
}

// Parse a normal clause declaration: "id: expr" or "id:- trait"
def tryClauseDecl(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(None))

    // Check for trait bound
    if (parser.eatToken(lex.Tag.`:-`)) {
      val trait_ = tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(t)) => t
        case _ => boundary.break(result(parser.invalidTerm("trait", "expected a trait after ':-'")))

      boundary.break(result(AstNode2(Tag.clause_trait_bound_decl, parser.currentSpan(), id, trait_)))
    }

    if (parser.eatToken(lex.Tag.`:`)) {
      // Regular declaration
      val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(e)) => e
        case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after ':'")))

      boundary.break(result(AstNode2(Tag.clause_decl, parser.currentSpan(), id, expr)))
    }

    result(AstNode1(Tag.clause_type_decl, parser.currentSpan(), id))
  }
}

// Parse an optional clause declaration: ".id: expr"
def tryClauseOptionalDecl(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.eatToken(lex.Tag.`.`)) boundary.break(result(None))

    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected an identifier after '.'")))

    if (!parser.eatToken(lex.Tag.`:`)) boundary
      .break(result(AstNode1(Tag.clause_type_decl_optional, parser.currentSpan(), id)))

    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after ':'")))

    result(AstNode2(Tag.clause_decl_optional, parser.currentSpan(), id, expr))
  }
}

// Parse a requires clause: "requires expr"
def tryRequiresClause(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.eatToken(lex.Tag.k_requires)) boundary.break(result(None))

    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'requires'")))

    result(AstNode1(Tag.requires_predicate, parser.currentSpan(), expr))
  }
}

// Parse an ensures clause: "ensures expr"
def tryEnsuresClause(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.eatToken(lex.Tag.k_ensures)) boundary.break(result(None))

    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'ensures'")))

    result(AstNode1(Tag.ensures_predicate, parser.currentSpan(), expr))
  }
}

// Parse a decreases clause: "decreases expr"
def tryDecreasesClause(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.eatToken(lex.Tag.k_decreases)) boundary.break(result(None))

    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'decreases'")))

    result(AstNode1(Tag.decreases, parser.currentSpan(), expr))
  }
}

// Parse clauses: "where clause*"
def tryClauses(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_where)) {
  boundary {
    tryMulti(parser, None, "parsing clauses", Rule(Tag.expr, tryClause)) match
      case Left(e) => boundary.break(result(e))
      case Right(nodes) => result(AstNodeN(Tag.clauses, parser.currentSpan(), nodes))
  }
}

// Parse return type: "-> expr"
def tryReturnType(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`->`)) {
  boundary {
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after '->'")))

    result(AstNode1(Tag.return_type, parser.currentSpan(), expr))
  }
}

// Parse gradual return type: "~> expr"
def tryGradualReturnType(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.`~>`)) {
  boundary {
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after '~>'")))

    result(AstNode1(Tag.gradual_return_type, parser.currentSpan(), expr))
  }
}

// fn_def -> fn id? ( param* ) ( return_type | gradual_return_type )? clauses? block
def tryFnDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_fn)) {
  boundary {
    // Optional function name
    val name = tryId(parser) match
      case Right(Some(id)) => id
      case _ => AstNode0(Tag.invalid, parser.currentSpan())
    var params: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    var returnType: AstNode = AstNode0(Tag.invalid, parser.currentSpan())

    val kind = parser.peekToken().tag match
      case lex.Tag.`(` => lex.Tag.`(`
      case lex.Tag.`<` => lex.Tag.`<`
      case _ => boundary.break(result(parser.invalidTerm("(", "expected '(' or '<' for parameter list")))

    params = tryMulti(parser, Some(kind), "parsing function parameters", Rule(Tag.param, tryParam)) match
      case Right(p) => AstNodeN(Tag.params, parser.currentSpan(), p)
      case Left(e) => boundary.break(result(e))

    tryReturnType(parser) match
      case Right(Some(rt)) => returnType = rt
      case _ => tryGradualReturnType(parser) match
          case Right(Some(rt)) => returnType = rt
          case _ => ()

    // Where clauses
    val clauses = tryClauses(parser) match
      case Right(Some(c)) => c
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    // Function body
    val stmtRules = Rule(Tag.statement, tryStatement, lex.Tag.`;`)
    val body = tryBlock(parser) match
      case Right(Some(b)) => b
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("block", "expected a function body")))

    result(AstNode5(Tag.fn_def, parser.currentSpan(), name, params, returnType, clauses, body))
  }
}

// struct_field -> id : expr (= expr)?
def tryStructField(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(None))

    if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(parser.invalidTerm(":", "expected ':' after field name")))

    val typ = tryExpr(parser) match
      case Right(Some(t)) => t
      case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

    // Default value
    var default: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    if (parser.eatToken(lex.Tag.`=`)) {
      default = tryExpr(parser) match
        case Right(Some(d)) => d
        case _ => AstNode0(Tag.invalid, parser.currentSpan())
    }

    result(AstNode3(Tag.struct_field, parser.currentSpan(), id, typ, default))
  }
}

// struct_def -> struct id? clauses? block(struct_field*)
def tryStructDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_struct)) {
  boundary {
    // Optional struct name
    val name = tryId(parser) match
      case Right(Some(id)) => id
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    // Where clauses
    val clauses = tryClauses(parser) match
      case Right(Some(c)) => c
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    // Struct body - fields
    val fieldRules = List(
      Rule(Tag.property, tryProperty),
      Rule(Tag.struct_field, tryStructField),
      Rule(Tag.definition, tryDefinition, lex.Tag.`;`),
      Rule(Tag.statement, tryStatement, lex.Tag.`;`)
    )

    tryMulti(parser, Some(lex.Tag.`{`), "parsing struct fields", fieldRules*) match
      case Right(fields) =>
        val structDefBody = AstNodeN(Tag.struct_def_body, parser.currentSpan(), fields)
        result(AstNode3(Tag.struct_def, parser.currentSpan(), name, clauses, structDefBody))
      case Left(e) => boundary.break(result(e))
  }
}

// enum_variant_with_struct -> id block(struct_field)
def tryEnumVariantWithStruct(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`{`)) boundary.break(result(None))

    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(None))

    tryMulti(parser, Some(lex.Tag.`{`), "parsing struct fields", Rule(Tag.struct_field, tryStructField)) match
      case Right(fields) => result(AstNode2(
          Tag.enum_variant_with_struct,
          parser.currentSpan(),
          id,
          AstNodeN(Tag.struct_def_body, parser.currentSpan(), fields)
        ))
      case Left(e) => boundary.break(result(e))
  }
}

// enum_variant_with_tuple -> id tuple
def tryEnumVariantWithTuple(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`(`)) boundary.break(result(None))

    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(None))

    tryMulti(parser, Some(lex.Tag.`(`), "parsing tuple types", Rule(Tag.expr, tryExpr)) match
      case Right(types) => result(AstNode2(
          Tag.enum_variant_with_tuple,
          parser.currentSpan(),
          id,
          AstNodeN(Tag.tuple, parser.currentSpan(), types)
        ))
      case Left(e) => boundary.break(result(e))
  }
}

// enum_variant_def_with_pattern -> id = pattern
def tryEnumVariantWithPattern(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`=`)) boundary.break(result(None))

    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(None))

    parser.eatToken(lex.Tag.`=`)

    val pattern = tryPattern(parser) match
      case Right(Some(p)) => p
      case _ => boundary.break(result(parser.invalidTerm("pattern", "expected a pattern after '='")))

    result(AstNode2(Tag.enum_variant_def_with_pattern, parser.currentSpan(), id, pattern))
  }
}

// enum_variant_with_sub_enum -> id. { enum_variant* }
def tryEnumVariantWithSubEnum(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`.`)) boundary.break(result(None))

    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(None))

    parser.eatToken(lex.Tag.`.`)

    tryMulti(parser, Some(lex.Tag.`{`), "parsing sub-enum variants", Rule(Tag.enum_variant, tryEnumVariant)) match
      case Right(variants) => result(AstNode2(
          Tag.enum_variant_with_sub_enum,
          parser.currentSpan(),
          id,
          AstNodeN(Tag.enum_def_body, parser.currentSpan(), variants)
        ))
      case Left(e) => boundary.break(result(e))
  }
}

// enum_variant -> id | enum_variant_with_struct | enum_variant_with_tuple | enum_variant_with_sub_enum | enum_variant_def_with_pattern
def tryEnumVariant(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    // Try complex variants first
    tryEnumVariantWithStruct(parser) match
      case Right(Some(node)) => boundary.break(result(node))
      case _ => ()

    tryEnumVariantWithTuple(parser) match
      case Right(Some(node)) => boundary.break(result(node))
      case _ => ()

    tryEnumVariantWithPattern(parser) match
      case Right(Some(node)) => boundary.break(result(node))
      case _ => ()

    tryEnumVariantWithSubEnum(parser) match
      case Right(Some(node)) => boundary.break(result(node))
      case _ => ()

    // Simple id variant
    tryId(parser) match
      case Right(Some(id)) => result(id)
      case _ => result(None)
  }
}

// enum_def -> enum id? clauses? block(enum_variant*)
def tryEnumDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_enum)) {
  boundary {
    // Optional enum name
    val name = tryId(parser) match
      case Right(Some(id)) => id
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    // Where clauses
    val clauses = tryClauses(parser) match
      case Right(Some(c)) => c
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    // Enum body - variants
    val variantRules = List(
      Rule(Tag.property, tryProperty),
      Rule(Tag.enum_variant, tryEnumVariant),
      Rule(Tag.definition, tryDefinition, lex.Tag.`;`),
      Rule(Tag.statement, tryStatement, lex.Tag.`;`)
    )

    tryMulti(parser, Some(lex.Tag.`{`), "parsing enum variants", variantRules*) match
      case Right(variants) =>
        val enumDefBody = AstNodeN(Tag.enum_def_body, parser.currentSpan(), variants)
        result(AstNode3(Tag.enum_def, parser.currentSpan(), name, clauses, enumDefBody))
      case Left(e) => boundary.break(result(e))
  }
}

// union_variant -> id : expr
def tryUnionVariant(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(None))

    if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(None))

    val type_ = tryExpr(parser) match
      case Right(Some(t)) => t
      case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

    result(AstNode2(Tag.union_variant, parser.currentSpan(), id, type_))
  }
}

// union_def -> union id? clauses? block(union_variant*)
def tryUnionDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_union)) {
  boundary {
    // Optional union name
    val name = tryId(parser) match
      case Right(Some(id)) => id
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    // Where clauses
    val clauses = tryClauses(parser) match
      case Right(Some(c)) => c
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    // Union body - variants
    val variantRules = List(
      Rule(Tag.property, tryProperty),
      Rule(Tag.union_variant, tryUnionVariant),
      Rule(Tag.definition, tryDefinition, lex.Tag.`;`),
      Rule(Tag.statement, tryStatement, lex.Tag.`;`)
    )

    tryMulti(parser, Some(lex.Tag.`{`), "parsing union variants", variantRules*) match
      case Right(variants) =>
        val unionDefBody = AstNodeN(Tag.union_def_body, parser.currentSpan(), variants)
        result(AstNode3(Tag.union_def, parser.currentSpan(), name, clauses, unionDefBody))
      case Left(e) => boundary.break(result(e))
  }
}

// impl_def -> impl expr (for expr)? clauses? block
def tryImplDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_impl)) {
  boundary {
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'impl'")))

    var forExpr: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    if (parser.eatToken(lex.Tag.k_for)) {
      forExpr = tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(e)) => e
        case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'for'")))
    }

    // Where clauses
    val clauses = tryClauses(parser) match
      case Right(Some(c)) => c
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    val stmtRules = List(
      Rule(Tag.property, tryProperty),
      Rule(Tag.definition, tryDefinition, lex.Tag.`;`),
      Rule(Tag.statement, tryStatement, lex.Tag.`;`)
    )

    println("impl_def: " + parser.peekToken().tag)
    val body = tryMulti(parser, Some(lex.Tag.`{`), "parsing impl body", stmtRules*) match
      case Right(list) => list
      case Left(e) => boundary.break(result(e))

    result(AstNode4(
      Tag.impl_def,
      parser.currentSpan(),
      expr,
      forExpr,
      clauses,
      AstNodeN(Tag.block, parser.currentSpan(), body)
    ))
  }
}

// derive -> derive expr* for expr clauses?
def tryDerive(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_derive)) {
  boundary {
    tryMulti(parser, None, "parsing derive expressions", Rule(Tag.expr, tryExpr)) match
      case Right(exprs) =>
        var forExpr: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
        if (parser.eatToken(lex.Tag.k_for)) {
          forExpr = tryExpr(parser, ExprOption(noRecordCall = true)) match
            case Right(Some(e)) => e
            case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'for'")))
        }

        // Where clauses
        val clauses = tryClauses(parser) match
          case Right(Some(c)) => c
          case _ => AstNode0(Tag.invalid, parser.currentSpan())

        result(
          AstNode3(Tag.derive, parser.currentSpan(), clauses, forExpr, AstNodeN(Tag.tuple, parser.currentSpan(), exprs))
        )

      case Left(e) => boundary.break(result(e))
  }
}

// extend -> extend expr (for expr)? clauses? block
def tryExtendDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_extend)) {
  boundary {
    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'extend'")))

    var forExpr: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    if (parser.eatToken(lex.Tag.k_for)) {
      forExpr = tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(e)) => e
        case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'for'")))
    }

    // Where clauses
    val clauses = tryClauses(parser) match
      case Right(Some(c)) => c
      case _ => AstNode0(Tag.invalid, parser.currentSpan())

    val stmtRules = List(
      Rule(Tag.property, tryProperty),
      Rule(Tag.definition, tryDefinition, lex.Tag.`;`),
      Rule(Tag.statement, tryStatement, lex.Tag.`;`)
    )

    val body = tryBlock(parser) match
      case Right(Some(b)) => b
      case _ => boundary.break(result(parser.invalidTerm("block", "expected an extension body")))

    result(AstNode4(Tag.extend, parser.currentSpan(), expr, forExpr, clauses, body))
  }
}

def tryModuleDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_mod)) {
  boundary {
    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected a module name after 'mod'")))

    val rules = List(
      Rule(Tag.property, tryProperty),
      Rule(Tag.definition, tryDefinition, lex.Tag.`;`),
      Rule(Tag.statement, tryStatement, lex.Tag.`;`)
    )

    tryBlock(parser) match
      case Right(Some(block)) => result(AstNode2(Tag.mod, parser.currentSpan(), id, block))
      case _ => result(AstNode1(Tag.mod_file, parser.currentSpan(), id))
  }
}

// typealias -> typealias id(<id | param*>)? = expr
def tryTypeAlias(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_typealias)) {
  boundary {
    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected a type alias name after 'typealias'")))

    var params: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    params = tryMulti(
      parser,
      Some(lex.Tag.`<`),
      "parsing type alias parameters",
      Rule(Tag.id, tryId),
      Rule(Tag.param, tryParam)
    ) match
      case Right(p) => AstNodeN(Tag.params, parser.currentSpan(), p)
      case Left(e) => boundary.break(result(e))

    if (!parser.eatToken(lex.Tag.`=`)) boundary
      .break(Left(ParseError.UnexpectedToken(lex.Tag.`=`, parser.peekToken(), "expected '=' after type alias name")))

    val expr = tryExpr(parser) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after '='")))

    result(AstNode3(Tag.typealias, parser.currentSpan(), id, params, expr))
  }
}

// newtype -> newtype id(<id | param*>)? = expr
def tryNewType(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_newtype)) {
  boundary {
    val id = tryId(parser) match
      case Right(Some(id)) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected a newtype name after 'newtype'")))

    var params: AstNode = AstNode0(Tag.invalid, parser.currentSpan())
    params = tryMulti(
      parser,
      Some(lex.Tag.`<`),
      "parsing newtype parameters",
      Rule(Tag.id, tryId),
      Rule(Tag.param, tryParam)
    ) match
      case Right(p) => AstNodeN(Tag.params, parser.currentSpan(), p)
      case Left(e) => boundary.break(result(e))

    if (!parser.eatToken(lex.Tag.`=`)) boundary
      .break(Left(ParseError.UnexpectedToken(lex.Tag.`=`, parser.peekToken(), "expected '=' after newtype name")))

    val expr = tryExpr(parser) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after '='")))

    result(AstNode3(Tag.newtype, parser.currentSpan(), id, params, expr))
  }
}

// Helper function to try parsing a definition
def tryDefinition(parser: Parser): ParseResult = withCtx(parser) {
  val token = parser.peekToken()
  token.tag match {
    case lex.Tag.k_fn => tryFnDef(parser)
    case lex.Tag.k_struct => tryStructDef(parser)
    case lex.Tag.k_enum => tryEnumDef(parser)
    case lex.Tag.k_union => tryUnionDef(parser)
    case lex.Tag.k_derive => tryDerive(parser)
    case lex.Tag.k_impl => tryImplDef(parser)
    case lex.Tag.k_extend => tryExtendDef(parser)
    case lex.Tag.k_mod => tryModuleDef(parser)
    case lex.Tag.k_typealias => tryTypeAlias(parser)
    case lex.Tag.k_newtype => tryNewType(parser)

    // case lex.Tag.`^` => tryAttribute(parser, tryDefinition)

    case lex.Tag.k_inline => tryPrefixTerm(parser, Tag.inline_def, lex.Tag.k_inline, tryDefinition)
    case lex.Tag.k_pub => tryPrefixTerm(parser, Tag.pub_def, lex.Tag.k_pub, tryDefinition)
    case lex.Tag.k_pure => tryPrefixTerm(parser, Tag.pure_def, lex.Tag.k_pure, tryDefinition)
    case lex.Tag.k_comptime => tryPrefixTerm(parser, Tag.comptime_def, lex.Tag.k_comptime, tryDefinition)
    case _ => result(None)
  }
}

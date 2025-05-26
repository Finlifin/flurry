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
      val id = parser.eatId() match
        case Some(id) => id
        case None => boundary
            .break(result(ParseError.UnexpectedToken(lex.Tag.id, parser.peekToken(), "parsing implicit parameter")))

      if (!parser.eatToken(lex.Tag.`:`)) boundary
        .break(result(parser.invalidTerm(":", "expected ':' after parameter name")))

      val typ = tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(t)) => t
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

      boundary.break(result(
        Ast.AttributeSetTrue("flurry_implicit_arg", Ast.ParamTyped(id, typ).withSpan(parser.currentSpan()))
      ))
    }

    if (parser.eatToken(lex.Tag.k_comptime)) {
      tryParam(parser) match
        case Right(Some(param)) => boundary.break(result(Ast.AttributeSetTrue(
            "flurry_comptime_arg",
            param.asInstanceOf[Ast.ParamTyped].withSpan(parser.currentSpan())
          )))
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("parameter", "expected a parameter after 'comptime'")))
    }

    if (parser.peek(lex.Tag.`*`, lex.Tag.k_self)) {
      parser.eatTokens(2)
      boundary.break(result(Ast.ParamSelfRef.withSpan(parser.currentSpan())))
    }

    if (parser.peek(lex.Tag.`*`, lex.Tag.k_itself)) {
      parser.eatTokens(2)
      boundary.break(result(Ast.ParamItselfRef.withSpan(parser.currentSpan())))
    }

    if (parser.eatToken(lex.Tag.k_self)) boundary.break(result(Ast.ParamSelf.withSpan(parser.currentSpan())))

    if (parser.eatToken(lex.Tag.k_itself)) boundary.break(result(Ast.ParamItself.withSpan(parser.currentSpan())))

    // ... id: expr (rest parameter)
    if (parser.peek(lex.Tag.`.`, lex.Tag.`.`, lex.Tag.`.`, lex.Tag.id)) {
      parser.eatTokens(3)
      val id = parser.eatId() match
        case Some(id) => id
        case _ => boundary
            .break(result(ParseError.UnexpectedToken(lex.Tag.id, parser.peekToken(), "parsing rest parameter")))

      if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(
        ParseError.UnexpectedToken(lex.Tag.`:`, parser.peekToken(), "varargs must be specified with a type")
      ))

      val typ = tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(t)) => t
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

      boundary.break(result(Ast.ParamRestBind(id, typ).withSpan(parser.currentSpan())))
    }

    // .id: expr (optional parameter)
    if (parser.peek(lex.Tag.`.`, lex.Tag.id)) {
      parser.eatTokens(1)
      val id = parser.eatId() match
        case Some(id) => id
        case _ => boundary.break(result(parser.invalidTerm("identifier", "parsing an optional parameter")))

      if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(Ast.ParamOptionalId(id).withSpan(parser.currentSpan())))

      val typ = tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(t)) => t
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

      val default =
        if (parser.eatToken(lex.Tag.`=`)) {
          tryExpr(parser, ExprOption(noRecordCall = true)) match
            case Right(Some(d)) => d
            case Left(e) => boundary.break(result(e))
            case _ => boundary.break(result(parser.invalidTerm("default value", "expected a default value after '='")))
        } else { Ast.Invalid.withSpan(parser.currentSpan()) }

      boundary.break(result(Ast.ParamOptional(id, typ, default).withSpan(parser.currentSpan())))
    }

    // Regular parameter
    if (parser.peek(lex.Tag.id)) {
      val id = parser.eatId() match
        case Some(id) => id
        case _ => boundary
            .break(result(ParseError.UnexpectedToken(lex.Tag.id, parser.peekToken(), "parsing parameter")))

      // Parameter with trait bound: id:- trait
      if (parser.eatToken(lex.Tag.`:-`)) {
        val trait_ = tryExpr(parser, ExprOption(noRecordCall = true)) match
          case Right(Some(t)) => t
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidTerm("trait", "expected a trait after ':-'")))

        boundary.break(result(Ast.ParamTraitBound(id, trait_).withSpan(parser.currentSpan())))
      }

      // Regular typed parameter: id: type
      if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(Ast.ParamId(id).withSpan(parser.currentSpan())))

      val typ = tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(t)) => t
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

      boundary.break(result(Ast.ParamTyped(id, typ).withSpan(parser.currentSpan())))
    }

    result(None)
  }
}

// Try to parse a where clause
def tryClause(parser: Parser): ParseResult = withCtx(parser) {
  val token = parser.peekToken()
  token.tag match {
    case lex.Tag.k_requires => tryRequiresClause(parser)
    case lex.Tag.k_ensures => tryEnsuresClause(parser)
    case lex.Tag.k_decreases => tryDecreasesClause(parser)
    case _ => tryParam(parser)
  }
}

// // Parse a normal clause declaration: "id: expr" or "id:- trait"
// def tryClauseDecl(parser: Parser): ParseResult = withCtx(parser) {
//   boundary {
//     val id = tryId(parser) match
//       case Right(Some(id)) => id
//       case _ => boundary.break(result(None))

//     // Check for trait bound
//     if (parser.eatToken(lex.Tag.`:-`)) {
//       val trait_ = tryExpr(parser, ExprOption(noRecordCall = true)) match
//         case Right(Some(t)) => t
//         case _ => boundary.break(result(parser.invalidTerm("trait", "expected a trait after ':-'")))

//       boundary
//         .break(result(Ast.ClauseTraitBoundDecl(id.asInstanceOf[Ast.Id].name, trait_).withSpan(parser.currentSpan())))
//     }

//     if (parser.eatToken(lex.Tag.`:`)) {
//       // Regular declaration
//       val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
//         case Right(Some(e)) => e
//         case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after ':'")))

//       boundary.break(result(Ast.ClauseDecl(id.asInstanceOf[Ast.Id].name, expr).withSpan(parser.currentSpan())))
//     }

//     result(Ast.ClauseTypeDecl(id.asInstanceOf[Ast.Id].name).withSpan(parser.currentSpan()))
//   }
// }

// // Parse an optional clause declaration: ".id: expr"
// def tryClauseOptionalDecl(parser: Parser): ParseResult = withCtx(parser) {
//   boundary {
//     if (!parser.eatToken(lex.Tag.`.`)) boundary.break(result(None))

//     val id = tryId(parser) match
//       case Right(Some(id)) => id
//       case _ => boundary.break(result(parser.invalidTerm("identifier", "expected an identifier after '.'")))

//     if (!parser.eatToken(lex.Tag.`:`)) boundary
//       .break(result(Tag.ClauseTypeDeclOptional(id.asInstanceOf[Ast.Id].name).withSpan(parser.currentSpan())))

//     val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
//       case Right(Some(e)) => e
//       case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after ':'")))

//     result(AstNode2(Tag.clause_decl_optional, parser.currentSpan(), id, expr))
//   }
// }

// Parse a requires clause: "requires expr"
def tryRequiresClause(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.eatToken(lex.Tag.k_requires)) boundary.break(result(None))

    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'requires'")))

    result(Ast.Requires(expr).withSpan(parser.currentSpan()))
  }
}

// Parse an ensures clause: "ensures expr"
def tryEnsuresClause(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.eatToken(lex.Tag.k_ensures)) boundary.break(result(None))

    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'ensures'")))

    result(Ast.Ensures(expr).withSpan(parser.currentSpan()))
  }
}

// Parse a decreases clause: "decreases expr"
def tryDecreasesClause(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.eatToken(lex.Tag.k_decreases)) boundary.break(result(None))

    val expr = tryExpr(parser, ExprOption(noRecordCall = true)) match
      case Right(Some(e)) => e
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'decreases'")))

    result(Ast.Decreases(expr).withSpan(parser.currentSpan()))
  }
}

// Parse clauses: "where clause*"
def tryClauses(parser: Parser): Either[ParseError, List[Ast]] =
  parser.enter()
  try
    if (parser.eatToken(lex.Tag.k_where)) tryMulti(parser, None, "parsing clauses", Rule("clause", tryClause)) match
      case Right(clauses) => Right(clauses)
      case Left(e) => Left(e)
    else Right(Nil)
  finally parser.exit()

// Parse return type: "-> expr"
def tryReturnType(parser: Parser): ParseResult =
  withCtx(parser, Some(lex.Tag.`->`))(tryExpr(parser, ExprOption(noRecordCall = true)))

// Parse gradual return type: "~> expr"
def tryGradualReturnType(parser: Parser): ParseResult =
  withCtx(parser, Some(lex.Tag.`~>`))(tryExpr(parser, ExprOption(noRecordCall = true)))

// fn_def -> fn id? ( param* ) ( return_type | gradual_return_type )? clauses? block
// fn_def -> fn id? ( param* ) ( return_type | gradual_return_type )? clauses? = expr
def tryFnDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_fn)) {
  boundary {
    // Optional function name
    val name = parser.eatId() match
      case Some(id) => id
      case _ => ""

    // Parse parameter list: ( ... ) or < ... >
    val kind = parser.peekToken().tag match
      case lex.Tag.`(` => lex.Tag.`(`
      case lex.Tag.`<` => lex.Tag.`<`
      case _ => boundary.break(result(parser.invalidTerm("(", "expected '(' or '<' for parameter list")))

    val params = tryMulti(parser, Some(kind), "parsing function parameters", Rule("function parameter", tryParam)) match
      case Right(params) => params
      case Left(e) => boundary.break(result(e))

    // Try return type: -> expr or ~> expr (optional)
    val returnType = tryReturnType(parser) match
      case Right(Some(rt)) => rt
      case _ => tryGradualReturnType(parser) match
          case Right(Some(rt)) => rt
          case _ => Ast.Invalid.withSpan(parser.currentSpan())

    // Where clauses (optional)
    val clauses = tryClauses(parser) match
      case Right(nodes) => nodes
      case Left(e) => boundary.break(result(e))

    // Function body: block or = expr
    val body =
      if (parser.peekToken().tag == lex.Tag.`=`) {
        parser.eatToken(lex.Tag.`=`)
        tryExpr(parser) match
          case Right(Some(expr)) => expr
          case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after '='")))
      } else {
        tryBlock(parser) match
          case Right(Some(b)) => b
          case Left(e) => boundary.break(result(e))
          case _ => boundary.break(result(parser.invalidTerm("block", "expected a function body")))
      }

    result(Ast.FunctionDef(name, params.toMList, returnType, clauses.toMList, body).withSpan(parser.currentSpan()))
  }
}

// struct_field -> id : expr (= expr)?
def tryStructField(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    val id = parser.eatId() match
      case Some(id) => id
      case _ => boundary.break(result(None))

    if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(parser.invalidTerm(":", "expected ':' after field name")))

    val typ = tryExpr(parser) match
      case Right(Some(t)) => t
      case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

    val default =
      if (parser.eatToken(lex.Tag.`=`)) tryExpr(parser) match
        case Right(Some(d)) => d
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("default value", "expected a default value after '='")))
      else Ast.Invalid.withSpan(parser.currentSpan())

    result(Ast.StructField(id, typ, default).withSpan(parser.currentSpan()))
  }
}

// struct_def -> struct id? clauses? block(struct_field*)
def tryStructDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_struct)) {
  boundary {
    val name = parser.eatId() match
      case Some(id) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected a struct name after 'struct'")))

    val clauses = tryClauses(parser) match
      case Right(cs) => cs
      case Left(e) => boundary.break(result(e))

    val fieldRules = List(
      Rule("struct property", tryProperty),
      Rule("struct field", tryStructField),
      Rule("definition", tryDefinition, lex.Tag.`;`),
      Rule("statement", tryStatement, lex.Tag.`;`)
    )

    tryMulti(parser, Some(lex.Tag.`{`), "parsing struct fields", fieldRules*) match
      case Right(fields) => result(Ast.StructDef(name, clauses.toMList, fields.toMList))
      case Left(e) => boundary.break(result(e))
  }
}

// enum_variant_with_struct -> id block(struct_field)
def tryEnumVariantWithStruct(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`{`)) boundary.break(result(None))

    val id = parser.eatId() match
      case Some(id) => id
      case _ => throw IllegalAccessException("reached unreachable code in tryEnumVariantWithStruct")

    tryMulti(parser, Some(lex.Tag.`{`), "parsing struct fields", Rule("struct fields", tryStructField)) match
      case Right(fields) => result(Ast.EnumVariantWithStruct(id, fields.toMList).withSpan(parser.currentSpan()))
      case Left(e) => boundary.break(result(e))
  }
}

// enum_variant_with_tuple -> id tuple
def tryEnumVariantWithTuple(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`(`)) boundary.break(result(None))

    val id = parser.eatId() match
      case Some(id) => id
      case _ => throw IllegalAccessException("reached unreachable code in tryEnumVariantWithTuple")

    tryMulti(parser, Some(lex.Tag.`(`), "parsing tuple types", Rule("tuple type expression", tryExpr)) match
      case Right(types) => result(Ast.EnumVariantWithTuple(id, types.toMList).withSpan(parser.currentSpan()))
      case Left(e) => boundary.break(result(e))
  }
}

// enum_variant_def_with_pattern -> id = pattern
def tryEnumVariantWithPattern(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`=`)) boundary.break(result(None))

    val id = parser.eatId() match
      case Some(id) => id
      case _ => throw IllegalAccessException("reached unreachable code in tryEnumVariantWithPattern")

    parser.eatToken(lex.Tag.`=`)

    val pattern = tryPattern(parser) match
      case Right(Some(p)) => p
      case _ => boundary.break(result(parser.invalidTerm("pattern", "expected a pattern after '='")))

    result(Ast.EnumVariantWithPattern(id, pattern).withSpan(parser.currentSpan()))
  }
}

// enum_variant_with_sub_enum -> id. { enum_variant* }
def tryEnumVariantWithSubEnum(parser: Parser): ParseResult = withCtx(parser) {
  boundary {
    if (!parser.peek(lex.Tag.id, lex.Tag.`.`, lex.Tag.`{`)) boundary.break(result(None))

    val id = parser.eatId() match
      case Some(id) => id
      case _ => throw IllegalAccessException("reached unreachable code in tryEnumVariantWithSubEnum")

    parser.eatToken(lex.Tag.`.`)

    tryMulti(parser, Some(lex.Tag.`{`), "parsing sub-enum variants", Rule("sub enum variants", tryEnumVariant)) match
      case Right(variants) => result(Ast.EnumVariantWithSubEnum(id, variants.toMList).withSpan(parser.currentSpan()))
      case Left(e) => boundary.break(result(e))
  }
}

// enum_variant -> id | enum_variant_with_struct | enum_variant_with_tuple | enum_variant_with_sub_enum | enum_variant_def_with_pattern | struct_field
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

    tryStructField(parser) match
      case Right(Some(node)) => node
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
    val name = parser.eatId() match
      case Some(id) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected an enum name after 'enum'")))

    val clauses = tryClauses(parser) match
      case Right(cs) => cs
      case Left(e) => boundary.break(result(e))

    val variantRules = List(
      Rule("enum property", tryProperty),
      Rule("enum variant", tryEnumVariant),
      Rule("definition", tryDefinition, lex.Tag.`;`),
      Rule("statement", tryStatement, lex.Tag.`;`)
    )

    tryMulti(parser, Some(lex.Tag.`{`), "parsing enum variants", variantRules*) match
      case Right(variants) =>
        result(Ast.EnumDef(name, clauses.toMList, variants.toMList).withSpan(parser.currentSpan()))
      case Left(e) => boundary.break(result(e))
  }
}

// union_variant -> id : expr
def tryUnionVariant(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.id), true) {
  boundary {
    val id = parser.eatId() match
      case Some(id) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected a union variant name")))

    if (!parser.eatToken(lex.Tag.`:`)) boundary.break(result(None))

    val type_ = tryExpr(parser) match
      case Right(Some(t)) => t
      case _ => boundary.break(result(parser.invalidTerm("type", "expected a type after ':'")))

    result(Ast.UnionVariant(id, type_).withSpan(parser.currentSpan()))
  }
}

// union_def -> union id? clauses? block(union_variant*)
def tryUnionDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_union)) {
  boundary {
    val name = parser.eatId() match
      case Some(id) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected a union name after 'union'")))

    val clauses = tryClauses(parser) match
      case Right(cs) => cs
      case Left(e) => boundary.break(result(e))

    val variantRules = List(
      Rule("union property", tryProperty),
      Rule("union variant", tryUnionVariant),
      Rule("definition", tryDefinition, lex.Tag.`;`),
      Rule("statement", tryStatement, lex.Tag.`;`)
    )

    tryMulti(parser, Some(lex.Tag.`{`), "parsing union variants", variantRules*) match
      case Right(variants) =>
        result(Ast.UnionDef(name, clauses.toMList, variants.toMList).withSpan(parser.currentSpan()))
      case Left(e) => boundary.break(result(e))
  }
}

// impl_def -> impl expr (for expr)? clauses? block
def tryImplDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_impl)) {
  boundary {
    val expr = tryExpr(parser) match
      case Right(Some(e)) => e
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected a type or a trait after 'impl'")))

    var forExpr =
      if (parser.eatToken(lex.Tag.k_for)) tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(e)) => e
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'for'")))
      else Ast.Invalid.withSpan(parser.currentSpan())

    val clauses = tryClauses(parser) match
      case Right(cs) => cs
      case Left(e) => boundary.break(result(e))

    tryMulti(
      parser,
      Some(lex.Tag.`{`),
      "parsing implementation body block",
      Rule("property", tryProperty),
      Rule("struct field", tryStructField),
      Rule("definition", tryDefinition, lex.Tag.`;`),
      Rule("statement", tryStatement, lex.Tag.`;`)
    ) match
      case Right(body) =>
        result(Ast.ImplDef(expr, forExpr, clauses.toMList, body.toMList).withSpan(parser.currentSpan()))
      case Left(e) => boundary.break(result(e))

  }
}

// derive -> derive expr* for expr clauses?
def tryDerive(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_derive)) {
  boundary {
    tryMulti(parser, None, "parsing derive expressions", Rule("trait expression", tryExpr)) match
      case Right(exprs) =>
        val forExpr =
          if (parser.eatToken(lex.Tag.k_for)) tryExpr(parser, ExprOption(noRecordCall = true)) match
            case Right(Some(e)) => e
            case Left(e) => boundary.break(result(e))
            case _ => boundary
                .break(result(parser.invalidTerm("expression", "expected an type expression after 'for'")))
          else Ast.Invalid.withSpan(parser.currentSpan())

        val clauses = tryClauses(parser) match
          case Right(cs) => cs
          case Left(e) => boundary.break(result(e))

        result(Ast.DeriveDef(exprs.toMList, forExpr, clauses.toMList).withSpan(parser.currentSpan()))

      case Left(e) => boundary.break(result(e))
  }
}

// extend -> extend expr (for expr)? clauses? block
def tryExtendDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_extend))(boundary {
  boundary {
    val expr = tryExpr(parser) match
      case Right(Some(e)) => e
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected a type or a trait after 'extend'")))

    var forExpr =
      if (parser.eatToken(lex.Tag.k_for)) tryExpr(parser, ExprOption(noRecordCall = true)) match
        case Right(Some(e)) => e
        case Left(e) => boundary.break(result(e))
        case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after 'for'")))
      else Ast.Invalid.withSpan(parser.currentSpan())

    val clauses = tryClauses(parser) match
      case Right(cs) => cs
      case Left(e) => boundary.break(result(e))

    tryMulti(
      parser,
      Some(lex.Tag.`{`),
      "parsing extension body block",
      Rule("property", tryProperty),
      Rule("struct field", tryStructField),
      Rule("definition", tryDefinition, lex.Tag.`;`),
      Rule("statement", tryStatement, lex.Tag.`;`)
    ) match
      case Right(body) =>
        result(Ast.ExtendDef(expr, forExpr, clauses.toMList, body.toMList).withSpan(parser.currentSpan()))
      case Left(e) => boundary.break(result(e))

  }
})

def tryModuleDef(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_mod))(boundary {
  val modName = parser.eatId() match
    case Some(id) => id
    case _ => boundary.break(result(parser.invalidTerm("module name", "expected a module name after 'mod'")))

  val clause = tryClauses(parser) match
    case Right(clauses) => clauses
    case Left(e) => boundary.break(result(e))

  if (!parser.peek(lex.Tag.`{`)) boundary
    .break(result(ParseError.UnexpectedToken(lex.Tag.`{`, parser.peekToken(), "expected '{' after module name")))

  val items = tryMulti(
    parser,
    Some(lex.Tag.`{`),
    "parsing module items",
    Rule("property", tryProperty),
    Rule("definition", tryDefinition, lex.Tag.`;`),
    Rule("statement", tryStatement, lex.Tag.`;`)
  ) match
    case Right(items) => items
    case Left(e) => boundary.break(result(e))

  result(Ast.ModuleDef(modName, clause.toMList, items.toMList).withSpan(parser.currentSpan()))
})

// typealias -> typealias id(<id | param*>)? = expr
def tryTypeAlias(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_typealias)) {
  boundary {
    val id = parser.eatId() match
      case Some(id) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected a type alias name after 'typealias'")))

    val params =
      tryMulti(parser, Some(lex.Tag.`<`), "parsing type alias parameters", Rule("typealias parameters", tryParam)) match
        case Right(p) => p
        case Left(e) => boundary.break(result(e))

    if (!parser.eatToken(lex.Tag.`=`)) boundary
      .break(Left(ParseError.UnexpectedToken(lex.Tag.`=`, parser.peekToken(), "expected '=' after type alias name")))

    val expr = tryExpr(parser) match
      case Right(Some(e)) => e
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after '='")))

    result(Ast.Typealias(id, params.toMList, expr).withSpan(parser.currentSpan()))
  }
}

// newtype -> newtype id(<id | param*>)? = expr
def tryNewType(parser: Parser): ParseResult = withCtx(parser, Some(lex.Tag.k_newtype)) {
  boundary {
    val id = parser.eatId() match
      case Some(id) => id
      case _ => boundary.break(result(parser.invalidTerm("identifier", "expected a newtype name after 'newtype'")))

    val params = tryMulti(
      parser,
      Some(lex.Tag.`<`),
      "parsing newtype parameters",
      Rule("newtype definition parameter", tryParam)
    ) match
      case Right(p) => p
      case Left(e) => boundary.break(result(e))

    if (!parser.eatToken(lex.Tag.`=`)) boundary
      .break(Left(ParseError.UnexpectedToken(lex.Tag.`=`, parser.peekToken(), "expected '=' after newtype name")))

    val expr = tryExpr(parser) match
      case Right(Some(e)) => e
      case Left(e) => boundary.break(result(e))
      case _ => boundary.break(result(parser.invalidTerm("expression", "expected an expression after '='")))

    result(Ast.Newtype(id, params.toMList, expr).withSpan(parser.currentSpan()))
  }
}

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

    case lex.Tag.k_inline => tryPrefixTerm(parser, Tag.inline_def, lex.Tag.k_inline, tryDefinition)
    case lex.Tag.k_pub => tryPrefixTerm(parser, Tag.pub_def, lex.Tag.k_pub, tryDefinition)
    case lex.Tag.k_pure => tryPrefixTerm(parser, Tag.pure_def, lex.Tag.k_pure, tryDefinition)
    case lex.Tag.k_comptime => tryPrefixTerm(parser, Tag.comptime_def, lex.Tag.k_comptime, tryDefinition)
    case _ => result(None)
  }
}

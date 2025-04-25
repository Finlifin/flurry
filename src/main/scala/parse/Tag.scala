package parse

enum Tag {
  case id
  case int
  case real
  case str
  case bool
  case char
  case symbol
  // int_extension -> int id
  case int_extension
  // real_extension -> real id
  case real_extension
  // str_extension -> str id
  case str_extension
  // char_extension -> char id
  case char_extension

  // expr_from_pattern -> < pattern >
  // list -> [ expr* ]
  // tuple -> ( expr, expr* )
  // parenthesis -> ( epxr )
  // unit -> ()
  // record -> { ( ...expr | property | expr )* }
  case expr_from_pattern
  case list
  case tuple
  case parenthesis
  case unit
  case record

  case bool_not

  // types
  case effect_qualified
  case error_qualified
  case optional
  case trait_object
  case pointer_type
  // forall_type -> forall<param*> expr
  case forall_type
  // for_type -> for<param*> expr
  case for_type

  case range_to
  case range_to_inclusive
  case range_from
  case range_from_to
  case range_from_to_inclusive

  case add
  case sub
  case mul
  case div
  case mod
  case add_add
  case bool_eq
  case bool_not_eq
  case bool_and
  case bool_or
  case bool_gt
  case bool_gt_eq
  case bool_lt
  case bool_lt_eq
  case bool_implies
  // bool_matches -> expr matches pattern
  case bool_matches

  // type_with -> expr : expr
  // subtype_with -> expr <: expr
  // trait_bound -> expr :- expr
  // field_method_bound -> expr :~ id : expr
  // declaration_bound -> expr :: id : expr
  case type_with
  case subtype_with
  case trait_bound
  case field_method_bound
  case declaration_bound

  // select -> expr . id
  // image -> expr ' id
  // pipe -> expr | expr
  // pipe_prepend -> expr |> call
  case select
  case image
  case pipe
  case pipe_prepend

  // deref -> expr . *
  // refer -> expr . ref
  // handler_apply -> expr . use(expr)
  // type_cast -> expr . as(expr)
  // as_dyn -> expr . dyn(expr)
  case deref
  case refer
  case handler_apply
  case type_cast
  case as_dyn

  // effect_elimination -> expr # { pattern_branch* }
  // error_elimination -> expr ! { (catch_branch | pattern_branch)* }
  // option_elimination -> expr ? block
  case effect_elimination
  case error_elimination
  case option_elimination
  case catch_branch

  // effect_emit -> expr #
  // error_throw -> expr !
  // option_unwrap -> expr ?
  case effect_emit
  case error_throw
  case option_unwrap

  // call -> expr ( arg* )
  // curry_call -> expr ' ( arg* )
  // diamond_call -> expr <arg*>
  // object_call -> expr { (... expr | property | expr)* }
  // index_call -> expr [ expr ]
  case call
  case curry_call
  case diamond_call
  case object_call
  case index_call

  case macro_call
  case prefix_macro_call

  case lambda

  case self_val // Renamed 'self' to 'self_val' as 'self' is a keyword in Scala
  case Self_type // Renamed 'Self' to 'Self_type' for clarity and to avoid potential conflicts
  case null_val // Renamed 'null' to 'null_val' as 'null' is a keyword in Scala
  case itself

  // -----------------//
  //    patterns      //
  // -----------------//
  // pattern_from_expr -> < expr >
  case pattern_from_expr

  // pattern_list -> [ pattern* ]
  case pattern_list

  // pattern_tuple -> ( pattern* )
  case pattern_tuple

  // property_pattern -> id : pattern
  case property_pattern
  // pattern_record -> { (id | id : pattern)*  }
  case pattern_record

  // patern_str_prefix -> str ++ id
  case pattern_str_prefix

  // pattern_symbold -> . id
  case pattern_symbol

  // pattern_not -> not pattern
  case pattern_not

  // pattern_async -> async pattern
  case pattern_async

  // pattern_type_bind -> ' id
  case pattern_type_bind

  // pattern_range_to -> .. pattern
  case pattern_range_to

  // pattern_range_to_inclusive -> ..= pattern
  case pattern_range_to_inclusive

  // pattern_list_rest_bind -> ... id
  case pattern_list_rest_bind

  // pattern_bitvec_0x -> 0x (int | id | ( id : pattern | pattern ))*
  case pattern_bitvec_0x

  // pattern_bitvec_0o -> 0o (int | ( id : pattern | pattern ))*
  case pattern_bitvec_0o

  // pattern_bitvec_0b -> 0b (int | ( id : pattern | pattern))*
  case pattern_bitvec_0b

  // pattern_option_some -> pattern ?
  case pattern_option_some

  // pattern_error_ok -> pattern !
  case pattern_error_ok

  // pattern_error_error -> error
  case pattern_error_error

  // pattern_range_from -> pattern ..
  case pattern_range_from

  // pattern_range_from_to -> pattern .. pattern
  case pattern_range_from_to

  // pattern_range_from_to_inclusive -> pattern ..= pattern
  case pattern_range_from_to_inclusive

  // pattern_or -> pattern or pattern
  case pattern_or

  // pattern_and_is -> pattern and expr is pattern
  case pattern_and_is

  // pattern_select -> pattern . id
  case pattern_select

  // pattern_select_all -> pattern . *
  case pattern_select_all

  // pattern_call -> pattern ( (id : pattern | pattern)* )
  case pattern_call

  // pattern_diamond_call -> pattern < (id : pattern | pattern)* >
  case pattern_diamond_call

  // pattern_object_call -> pattern { (id | id: pattern)* }
  case pattern_object_call

  // pattern_as_bind -> pattern as id
  case pattern_as_bind

  // pattern_if_guard -> pattern if expr
  case pattern_if_guard

  // branch -> pattern => statement | block,
  case branch
  case branches

  // if_is_pattern -> if expr is pattern block (else (if | block))?
  case if_is_pattern

  // if_is_match -> if expr is { branch* }
  case if_is_match

  // post_match -> expr match { branch }
  case post_match

  // while_is_pattern -> while label? expr is pattern block
  case while_is_pattern

  // while_is_match -> while label? expr is { branch* }
  case while_is_match

  // ----------------//
  //   statements    //
  // ----------------//
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
  // exclude_path -> not id
  // package_path -> @ mod_path
  // path_as_bind -> mod_path as id
  case use_statement
  case path_select
  case path_select_multi
  case path_select_all
  case super_path
  case exclude_path
  case package_path
  case path_as_bind

  case mod_file
  // if_guard -> if expr
  // label -> id
  // -- 控制流操作
  // break -> break label? if_guard?
  // continue -> continue label? if_guard?
  // return -> return expr? if_guard?
  case break_statement
  case continue_statement
  case return_statement

  // let_decl -> let pattern (: expr)? (= expr)?
  // const_decl -> const pattern (: expr)? (= expr)?
  case let_decl
  case const_decl

  // expr_statement -> expr
  // assign -> expr = expr
  // assign_add -> expr += expr
  // assign_mul -> expr *= expr
  // assign_sub -> expr -= expr
  // assign_div -> expr /= expr
  // assign_mod -> expr %= expr
  case expr_statement
  case assign
  case assign_add
  case assign_mul
  case assign_sub
  case assign_div
  case assign_mod

  // -- 这几个用于形式化验证
  // asserts -> asserts expr
  // assumes -> assumes expr
  // invariant -> invariant expr
  // decreases -> decreases expr
  // -- 数据结构specification
  // axiom -> axiom expr
  case asserts
  case assumes
  case invariant
  case decreases
  case axiom

  // -- 基本分支语句
  // condition_branch -> expr => stmt | block
  // when -> when { condition_branch* }
  // else -> ( else block ) | ( else if)
  // if -> if expr block else?label -> : id
  case condition_branch
  case when_statement
  case if_statement

  // -- 基本循环语句
  // for_loop -> for label? pattern in expr block
  // while_loop -> while label? expr block
  // -- 如 whie:loop_example true { ... }, 其中loop_example是label名，并且这是一个死循环
  case for_loop
  case while_loop

  // 块
  case block
  case do_block
  case unsafe_block
  case comptime_block
  case async_block
  case atomic_block

  // typealias -> typealias<param*> id = expr
  case typealias
  // newtype -> newtype<param*> id = expr
  case newtype

  // ---------------//
  //  definitions   //
  // ---------------//
  // clauses -> where clause*
  case clauses
  // id : expr
  case clause_decl
  // .id : expr
  case clause_decl_optional
  // id
  case clause_type_decl
  // .id
  case clause_type_decl_optional
  // id :- expr
  case clause_trait_bound_decl
  // requires expr
  case requires_predicate
  // ensures expr
  case ensures_predicate
  // outcome (P expr)*
  case outcome_predicate

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
  // param_implicit -> implicit id: expr
  case param_optional
  case param_typed
  case param_trait_bound
  case param_id
  case param_optional_id
  case param_self
  case param_self_ref
  case param_itself
  case param_itself_ref
  case param_rest_bind
  case param_implicit
  case params

  // return_type -> -> expr
  case return_type
  // gradual_return_type -> ~> expr
  case gradual_return_type

  // fn_def -> fn id? ( param* ) ( return_type | gradual_return_type )? clauses? block
  case fn_def
  // effect_def -> effect id? ( param* ) ( return_type | gradual_return_type )? clauses? block
  case effect_def
  // diamond_fn_def -> diamond id? ( param* ) ( return_type | gradual_return_type )? clauses? block
  case diamond_fn_def
  // handles_def -> handles id ( param* ) ( return_type | gradual_return_type )? clauses? block
  case handles_def

  // impl_def -> impl expr (for expr)? clauses? block
  // derive -> derive expr* for expr clauses?
  // extend -> extend expr (for expr)? clauses? block
  case impl_def
  case derive
  case extend

  // struct_field -> id : expr (= expr)?
  // struct_def -> struct id? clauses? block(struct_field*)
  case struct_field
  case struct_def_body
  case struct_def

  // enum_variant_with_struct -> id block(struct_field)
  // enum_variant_with_tuple -> id tuple
  // enum_variant_with_sub_enum -> id . block(enum_variant)
  // enum_variant_def_with_pattern -> id = pattern
  // enum_variant -> id | enum_variant_with_struct | enum_variant_with_tuple | enum_variant_with_sub_enum | enum_variant_def_with_pattern
  // enum_def -> enum id? clauses? block(enum_variant*)
  case enum_variant_with_struct
  case enum_variant_with_tuple
  case enum_variant_with_sub_enum
  case enum_variant_def_with_pattern
  case enum_def_body
  case enum_def

  // union_variant -> id : expr
  // union_def -> union id? clauses? block(union_variant*)
  case union_variant
  case union_def_body
  case union_def

  // pub term
  case pub_def
  // verified term
  case verified_def
  // unsafe term
  case unsafe_def
  // ^expr term
  case attribute
  // property -> .id expr
  case property
  // property_assign -> .id = expr
  case property_assign
  // inline term
  case inline_def
  // pure term
  case pure_def
  // comptime term
  case comptime_def

  // expand_items -> ... expr
  case expand_items
  // pair -> id: term
  case pair

  case file_scope

  // a kind of ast node
  case expr
  case statement
  case pattern
  case definition
  case mod_path
  case param
  case enum_variant

  case invalid
}

object Tag {
  // If you need the 'into' functionality (getting the ordinal value), you can use the .ordinal method on a TagType instance in Scala.
  // For example: TagType.id.ordinal will give you the index of 'id'.

  // If you need the 'toString' functionality, Scala enums already have a default toString method.
  // If you need custom toString, you can override it within the enum definition (more complex for enum in Scala 2/early Scala 3, simpler in latest Scala 3).
  // For basic cases, the default toString should suffice which will return the name of the case, e.g., "id".
}

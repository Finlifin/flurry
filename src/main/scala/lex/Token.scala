package lex

enum Tag {
  // operators
  case `+`
  case `+=`
  case `++`
  case `_+_` // " + "
  case `<`
  case `<=`
  case `_<_` // " < "
  case `>`
  case `>=`
  case `_>_` // " > "
  case `!`
  case `!=`
  case `-`
  case `->`
  case `-=`
  case `_-_` // " - "
  case `.`
  case `..=`
  case `:`
  case `::`
  case `:~`
  case `:-`
  case `*`
  case `*=`
  case `_*_` // " * "
  case `/`
  case `/=`
  case `_/_` // " / "
  case `%`
  case `%=`
  case `_%_` // " % "
  case `=`
  case `=>`
  case `==`
  case `==>`
  case `~`
  case `~>`
  case `|`
  case `|>`
  case `#`
  case `?`
  case `\\`
  case `&`
  case `[`
  case `]`
  case `(`
  case `)`
  case `{`
  case `}`
  case `,`
  case `'`
  case `;`
  case `^`
  case `$`
  case `@`

  // primitive literals
  case str
  case int
  case real
  case char

  // keywords
  case k_and
  case k_as
  case k_asserts
  case k_assumes
  case k_async
  case k_atomic
  case k_axiom
  case k_await
  case k_bool
  case k_break
  case k_case
  case k_catch
  case k_comptime
  case k_const
  case k_continue
  case k_decreases
  case k_define
  case k_derive
  case k_do
  case k_dyn
  case k_effect
  case k_else
  case k_ensures
  case k_enum
  case k_error
  case k_exists
  case k_extend
  case k_extern
  case k_false
  case k_fn
  case k_Fn
  case k_for
  case k_forall
  case k_ghost
  case k_handles
  case k_if
  case k_impl
  case k_implicit
  case k_in
  case k_inline
  case k_invariant
  case k_is
  case k_itself
  case k_lemma
  case k_let
  case k_match
  case k_matches
  case k_move
  case k_mod
  case k_mut
  case k_newtype
  case k_not
  case k_null
  case k_opaque
  case k_opens
  case k_or
  case k_outcomes
  case k_perform
  case k_predicate
  case k_private
  case k_pub
  case k_pure
  case k_ref
  case k_refines
  case k_requires
  case k_resume
  case k_return
  case k_self
  case k_Self
  case k_spec
  case k_static
  case k_struct
  case k_test
  case k_trait
  case k_true
  case k_typealias
  case k_union
  case k_unsafe
  case k_use
  case k_using
  case k_verified
  case k_when
  case k_while
  case k_where

  // others
  case id
  case macro_content
  case comment
  case invalid
  case sof
  case eof // End of file
}

object Tag {
  val keywords: Map[String, Tag] = Map(
    "and" -> Tag.k_and,
    "as" -> Tag.k_as,
    "asserts" -> Tag.k_asserts,
    "assumes" -> Tag.k_assumes,
    "async" -> Tag.k_async,
    "atomic" -> Tag.k_atomic,
    "axiom" -> Tag.k_axiom,
    "await" -> Tag.k_await,
    "bool" -> Tag.k_bool,
    "break" -> Tag.k_break,
    "case" -> Tag.k_case,
    "catch" -> Tag.k_catch,
    "comptime" -> Tag.k_comptime,
    "const" -> Tag.k_const,
    "continue" -> Tag.k_continue,
    "decreases" -> Tag.k_decreases,
    "define" -> Tag.k_define,
    "derive" -> Tag.k_derive,
    "do" -> Tag.k_do,
    "dyn" -> Tag.k_dyn,
    "effect" -> Tag.k_effect,
    "else" -> Tag.k_else,
    "ensures" -> Tag.k_ensures,
    "enum" -> Tag.k_enum,
    "error" -> Tag.k_error,
    "exists" -> Tag.k_exists,
    "extend" -> Tag.k_extend,
    "extern" -> Tag.k_extern,
    "false" -> Tag.k_false,
    "fn" -> Tag.k_fn,
    "Fn" -> Tag.k_Fn,
    "for" -> Tag.k_for,
    "forall" -> Tag.k_forall,
    "ghost" -> Tag.k_ghost,
    "handles" -> Tag.k_handles,
    "if" -> Tag.k_if,
    "impl" -> Tag.k_impl,
    "implicit" -> Tag.k_implicit,
    "in" -> Tag.k_in,
    "inline" -> Tag.k_inline,
    "invariant" -> Tag.k_invariant,
    "is" -> Tag.k_is,
    "itself" -> Tag.k_itself,
    "lemma" -> Tag.k_lemma,
    "let" -> Tag.k_let,
    "match" -> Tag.k_match,
    "matches" -> Tag.k_matches,
    "move" -> Tag.k_move,
    "mod" -> Tag.k_mod,
    "mut" -> Tag.k_mut,
    "newtype" -> Tag.k_newtype,
    "not" -> Tag.k_not,
    "null" -> Tag.k_null,
    "opaque" -> Tag.k_opaque,
    "opens" -> Tag.k_opens,
    "or" -> Tag.k_or,
    "outcomes" -> Tag.k_outcomes,
    "perform" -> Tag.k_perform,
    "predicate" -> Tag.k_predicate,
    "private" -> Tag.k_private,
    "pub" -> Tag.k_pub,
    "pure" -> Tag.k_pure,
    "ref" -> Tag.k_ref,
    "refines" -> Tag.k_refines,
    "requires" -> Tag.k_requires,
    "resume" -> Tag.k_resume,
    "return" -> Tag.k_return,
    "self" -> Tag.k_self,
    "Self" -> Tag.k_Self,
    "spec" -> Tag.k_spec,
    "static" -> Tag.k_static,
    "struct" -> Tag.k_struct,
    "test" -> Tag.k_test,
    "trait" -> Tag.k_trait,
    "true" -> Tag.k_true,
    "typealias" -> Tag.k_typealias,
    "union" -> Tag.k_union,
    "unsafe" -> Tag.k_unsafe,
    "use" -> Tag.k_use,
    "using" -> Tag.k_using,
    "verified" -> Tag.k_verified,
    "when" -> Tag.k_when,
    "while" -> Tag.k_while,
    "where" -> Tag.k_where
  )
}

case class Token(tag: Tag, from: Int, to: Int)

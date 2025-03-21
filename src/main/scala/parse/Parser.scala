package parse

import lex.Token
import vfs.VfsNode
import scala.collection.mutable.ListBuffer

class Parser(var file: VfsNode) {
  var src: String = file.src.get
  var tokens: ListBuffer[Token] = file.tokens.get
  var ast: Ast = file.ast.get

  var cursor: Int = 0
  var cursors: ListBuffer[Int] = ListBuffer(0)

  def peek(expected: Token*): Boolean = {
    // check if forward tokens is in expected
    if (cursor + 1 + expected.length > tokens.length) {
      false
    } else {
      var result = true
      var i = 0
      while (result && i < expected.length) {
        if (tokens(cursor + 1 + i) != expected(i)) {
          result = false
        }
        i += 1
      }
      result
    }
  }

  def eatToken(expected: Token): Boolean = {
    if (cursor + 1 >= tokens.length) {
      false
    } else {
      if (tokens(cursor + 1) == expected) {
        cursor += 1
        true
      } else {
        false
      }
    }
  }

  // unchecked
  def eatTokens(amount: Int) = {
    cursor += amount
  }

  def nextToken(): Token = {
    if (cursor + 1 >= tokens.length) {
      lex.Token(lex.Tag.eof, 0, 0)
    } else {
      cursor += 1
      tokens(cursor)
    }
  }

  def peekToken(): Token = {
    if (cursor + 1 >= tokens.length) {
      lex.Token(lex.Tag.eof, 0, 0)
    } else {
      tokens(cursor + 1)
    }
  }

  def enter() = {
    cursors += cursor
  }

  def exit() = {
    if (cursors.length > 0) {
      cursors.remove(cursors.length - 1)
    }
  }
}

enum ParseError {
  case UnexpectedToken(expected: Option[Token], found: Token, when: String)
  case InvalidTerm(exppected: String, found: Token, when: String)
}

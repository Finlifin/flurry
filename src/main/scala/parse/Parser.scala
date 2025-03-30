package parse

import lex.Token
import vfs.VfsNode
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import _root_.parse.ParseError.UnexpectedToken

class Parser(var file: VfsNode) {
  var src: String = file.src.get
  var tokens: ListBuffer[Token] = file.tokens.get
  var ast: Option[Ast] = file.ast

  var cursor: Int = 0
  var cursors: Stack[Int] = Stack(0)

  def parse(): Ast = {
    val result = tryFileScope(this)
    var ast = Ast()
    result match {
      case Right(Some(node)) => ast.root = node
      case Left(e)           => handlerError(e)
      case _                 => {}
    }
    ast
  }

  def peek(expected: lex.Tag*): Boolean = {
    // check if forward tokens is in expected
    if (cursor + 1 + expected.length > tokens.length) {
      false
    } else {
      var result = true
      var i = 0
      for (i <- 0 until expected.length if result) {
        if (
          cursor + 1 + i < tokens.length && tokens(
            cursor + 1 + i
          ).tag != expected(i)
        ) {
          result = false
        }
      }
      result
    }
  }

  def eatToken(expected: lex.Tag): Boolean = {
    if (cursor + 1 >= tokens.length) {
      false
    } else {
      if (tokens(cursor + 1).tag == expected) {
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

  inline def srcContentT(token: Token): String =
    src.substring(token.from, token.to)

  def currentToken(): Token = {
    if (cursor >= tokens.length) {
      lex.Token(lex.Tag.eof, 0, 0)
    } else {
      tokens(cursor)
    }
  }

  def getToken(index: Int): Token = {
    if (index >= tokens.length) {
      lex.Token(lex.Tag.eof, 0, 0)
    } else {
      tokens(index)
    }
  }

  def currentSpan(): Span =
    // file.reportSpan(
    //   getToken(cursors.top + 1),
    //   getToken(cursor),
    //   vfs.ErrorKind.Info,
    //   errors.testingError,
    //   s"create span: (${cursors.top} ${cursor}), while ${cursors}",
    //   3,
    // )
    
    Span(cursors.top + 1, cursor)

  def enter(): Unit = {
    cursors.push(cursor)
  }

  def exit(): Unit = {
    cursors.pop()
  }

  def fallback(): Unit = {
    if (cursors.nonEmpty) {
      cursor = cursors.top
    }
  }

  def invalidTerm(expected: String, when: String): ParseError = {
    val token = peekToken()
    ParseError.InvalidTerm(expected, token, when)
  }

  def handlerError(error: ParseError): Unit = {
    error match {
      case ParseError.UnexpectedToken(expected, found, when) => {
        file.report(
          found,
          vfs.ErrorKind.Error,
          error,
          s"expected `${expected.toString()}`, found `${found.tag.toString()}`, while ${when}",
          3
        )
      }
      case ParseError.InvalidTerm(expected, found, when) => {
        file.report(
          found,
          vfs.ErrorKind.Error,
          error,
          s"expected a(n) ${expected}, while ${when}",
          3
        )
      }
      case _ => ()
    }
  }
}

enum ParseError extends errors.FlurryError {
  case UnexpectedToken(expected: lex.Tag, found: Token, when: String)
  case InvalidTerm(expected: String, found: Token, when: String)

  case MeetRecordStart
}

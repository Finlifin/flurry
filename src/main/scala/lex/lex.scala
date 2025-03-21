package lex

enum LexError {
  case UnexpectedEOF
  case InvalidChar
  case InvalidString
  case InvalidIdentifier
}

class Lexer(src: String) {
  var cursor: Int = 0
  // todo err: Unit = () // Scala Unit is like void, and defaults to ()

  private def token(tag: Tag, from: Int, to: Int): Token =
    Token(tag, from, to)

  def next(): Token = {
    if (cursor >= src.length) return token(Tag.eof, src.length, src.length)

    val oldCursor = cursor

    // 跳过空白字符
    skipWhitespace()
    if (cursor >= src.length) return token(Tag.eof, src.length, src.length)

    val c = src(cursor)

    c match {
      case '^' =>
        cursor += 1
        token(Tag.`^`, oldCursor, cursor)

      case '.' =>
        cursor += 1
        token(Tag.`.`, oldCursor, cursor)

      case '\'' =>
        if (cursor + 2 < src.length && src(cursor + 2) == '\'') {
          // 'n' => char
          cursor += 3
          token(Tag.char, oldCursor, cursor)
        } else if (
          cursor + 3 < src.length && src(cursor + 1) == '\\' && src(
            cursor + 3
          ) == '\''
        ) {
          // '\n' => char
          cursor += 4
          token(Tag.char, oldCursor, cursor)
        } else {
          // '   { ANY } => macro_content
          // TODO: 实现宏内容处理
          cursor += 1
          token(Tag.`'`, oldCursor, cursor)
        }

      case '@' =>
        cursor += 1
        token(Tag.`@`, oldCursor, cursor)

      case '\\' =>
        cursor += 1
        token(Tag.`\\`, oldCursor, cursor)

      case '&' =>
        cursor += 1
        token(Tag.`&`, oldCursor, cursor)

      case '(' =>
        cursor += 1
        token(Tag.`(`, oldCursor, cursor)

      case ')' =>
        cursor += 1
        token(Tag.`)`, oldCursor, cursor)

      case '[' =>
        cursor += 1
        token(Tag.`[`, oldCursor, cursor)

      case ']' =>
        cursor += 1
        token(Tag.`]`, oldCursor, cursor)

      case ';' =>
        cursor += 1
        token(Tag.`;`, oldCursor, cursor)

      case ',' =>
        cursor += 1
        token(Tag.`,`, oldCursor, cursor)

      case '?' =>
        cursor += 1
        token(Tag.`?`, oldCursor, cursor)

      case '~' =>
        if (peek('>')) {
          cursor += 2
          token(Tag.`~>`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`~`, oldCursor, cursor)
        }

      case '#' =>
        cursor += 1
        token(Tag.`#`, oldCursor, cursor)

      case '|' =>
        if (peek('>')) {
          cursor += 2
          token(Tag.`|>`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`|`, oldCursor, cursor)
        }

      case '>' =>
        if (isSeparated()) {
          cursor += 2
          token(Tag.`_>_`, oldCursor, cursor)
        } else if (peek('=')) {
          cursor += 2
          token(Tag.`>=`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`>`, oldCursor, cursor)
        }

      case '<' =>
        if (isSeparated()) {
          cursor += 2
          token(Tag.`_<_`, oldCursor, cursor)
        } else if (peek('=')) {
          cursor += 2
          token(Tag.`<=`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`<`, oldCursor, cursor)
        }

      case '+' =>
        if (isSeparated()) {
          cursor += 2
          token(Tag.`_+_`, oldCursor, cursor)
        } else if (peek('=')) {
          cursor += 2
          token(Tag.`+=`, oldCursor, cursor)
        } else if (peek('+')) {
          cursor += 2
          token(Tag.`++`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`+`, oldCursor, cursor)
        }

      case '-' =>
        if (isSeparated()) {
          cursor += 2
          token(Tag.`_-_`, oldCursor, cursor)
        } else if (peek('=')) {
          cursor += 2
          token(Tag.`-=`, oldCursor, cursor)
        } else if (peek('>')) {
          cursor += 2
          token(Tag.`->`, oldCursor, cursor)
        } else if (peek('-')) {
          // 处理单行注释
          cursor += 2
          while (cursor < src.length && src(cursor) != '\n') {
            cursor += 1
          }
          if (cursor < src.length) cursor += 1 // 跳过换行符
          token(Tag.comment, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`-`, oldCursor, cursor)
        }

      case '!' =>
        if (peek('=')) {
          cursor += 2
          token(Tag.`!=`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`!`, oldCursor, cursor)
        }

      case '=' =>
        if (peek('=')) {
          if (cursor + 2 < src.length && src(cursor + 2) == '>') {
            cursor += 3
            token(Tag.`==>`, oldCursor, cursor)
          } else {
            cursor += 2
            token(Tag.`==`, oldCursor, cursor)
          }
        } else if (peek('>')) {
          cursor += 2
          token(Tag.`=>`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`=`, oldCursor, cursor)
        }

      case '%' =>
        if (isSeparated()) {
          cursor += 2
          token(Tag.`_%_`, oldCursor, cursor)
        } else if (peek('=')) {
          cursor += 2
          token(Tag.`%=`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`%`, oldCursor, cursor)
        }

      case '{' =>
        if (peek('-')) {
          // 处理多行注释 {- ... -}
          cursor += 2
          var nesting = 1
          while (cursor < src.length && nesting > 0) {
            if (cursor + 1 < src.length) {
              if (src(cursor) == '{' && src(cursor + 1) == '-') {
                nesting += 1
                cursor += 2
              } else if (src(cursor) == '-' && src(cursor + 1) == '}') {
                nesting -= 1
                cursor += 2
              } else {
                cursor += 1
              }
            } else {
              cursor += 1
            }
          }
          token(Tag.comment, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`{`, oldCursor, cursor)
        }

      case '}' =>
        cursor += 1
        token(Tag.`}`, oldCursor, cursor)

      case ':' =>
        if (peek('-')) {
          cursor += 2
          token(Tag.`:-`, oldCursor, cursor)
        } else if (peek(':')) {
          cursor += 2
          token(Tag.`::`, oldCursor, cursor)
        } else if (peek('~')) {
          cursor += 2
          token(Tag.`:~`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`:`, oldCursor, cursor)
        }

      case '*' =>
        if (isSeparated()) {
          cursor += 2
          token(Tag.`_*_`, oldCursor, cursor)
        } else if (peek('=')) {
          cursor += 2
          token(Tag.`*=`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`*`, oldCursor, cursor)
        }

      case '/' =>
        if (isSeparated()) {
          cursor += 2
          token(Tag.`_/_`, oldCursor, cursor)
        } else if (peek('=')) {
          cursor += 2
          token(Tag.`/=`, oldCursor, cursor)
        } else {
          cursor += 1
          token(Tag.`/`, oldCursor, cursor)
        }

      case '_' =>
        cursor += 1
        token(Tag.underscore, oldCursor, cursor)

      case '$' =>
        cursor += 1
        token(Tag.`$`, oldCursor, cursor)

      case c if isIdentifierStart(c) =>
        recognizeId()

      case '`' =>
        recognizeArbitaryId()

      case c if c.isDigit =>
        recognizeDigit()

      case '"' =>
        recognizeStr()

      case _ =>
        cursor += 1
        token(Tag.invalid, oldCursor, cursor)
    }
  }

  private def skipWhitespace(): Unit = {
    while (cursor < src.length && " \t\n\r".contains(src(cursor))) {
      cursor += 1
    }
  }

  private def isIdentifierStart(c: Char): Boolean = {
    c.isLetter
  }

  private def isIdentifierPart(c: Char): Boolean = {
    c.isLetterOrDigit || c == '_'
  }

  private def peek(char: Char): Boolean = {
    cursor + 1 < src.length && src(cursor + 1) == char
  }

  private def isSeparated(): Boolean = {
    cursor > 0 && cursor + 1 < src.length &&
    src(cursor - 1) == ' ' && src(cursor + 1) == ' '
  }

  private def recognizeStr(): Token = {
    val oldCursor = cursor
    cursor += 1 // 跳过开始的双引号 "

    while (cursor < src.length) {
      val c = src(cursor)

      if (c == '"' && (cursor == 0 || src(cursor - 1) != '\\')) {
        cursor += 1 // 包含结束的双引号
        return token(Tag.str, oldCursor, cursor)
      }

      cursor += 1
    }

    // 如果没有找到结束的双引号，返回无效token
    return token(Tag.invalid, oldCursor, cursor)
  }

  private def recognizeId(): Token = {
    val b = src.getBytes
    val oldCursor = cursor
    while (true) {
      cursor += 1
      if (cursor >= src.length) {
        val identifier = src.substring(oldCursor, cursor)
        return Tag.keywords.get(identifier) match {
          case Some(kw) => token(kw, oldCursor, cursor)
          case None     => token(Tag.id, oldCursor, cursor)
        }
      }
      val c = b(cursor).toChar

      c match {
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' |
            'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' |
            'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' |
            'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' |
            'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '0' | '1' | '2' |
            '3' | '4' | '5' | '6' | '7' | '8' | '9' | '_' => {}
        case _ =>
          val identifier = src.substring(oldCursor, cursor)
          return Tag.keywords.get(identifier) match {
            case Some(kw) => token(kw, oldCursor, cursor)
            case None     => token(Tag.id, oldCursor, cursor)
          }
      }
    }
    token(Tag.invalid, oldCursor, cursor) // Should not reach here
  }

  // 23 => int(23)
  // 23.0 => real(23.0)
  private def recognizeDigit(): Token = {
    val b = src.getBytes
    val oldCursor = cursor

    while (true) {
      cursor += 1
      if (cursor >= src.length) return token(Tag.int, oldCursor, cursor)
      val c = b(cursor).toChar
      c match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
            '_' => {}
        case '.' => {
          // 如果b[self.cursor + 1] == '0'...'9'，则是real
          if (
            cursor + 1 < src.length && b(cursor + 1).toChar >= '0' && b(
              cursor + 1
            ).toChar <= '9'
          ) {
            cursor += 1
            while (true) {
              cursor += 1
              if (cursor >= src.length)
                return token(Tag.real, oldCursor, cursor)
              val c_ = b(cursor).toChar
              if (c_ < '0' || c_ > '9')
                return token(
                  Tag.real,
                  oldCursor,
                  cursor
                ) // Break and return real token
            }
            token(Tag.real, oldCursor, cursor) // Should not reach here
          } else {
            // 如果不是数字，则是int
            return token(Tag.int, oldCursor, cursor)
          }
        }
        case _ => return token(Tag.int, oldCursor, cursor)
      }
    }
    token(Tag.invalid, oldCursor, cursor) // Should not reach here
  }

  private def recognizeArbitaryId(): Token = {
    val oldCursor = cursor
    cursor += 1 // 跳过开始的反引号 `

    while (cursor < src.length) {
      val c = src(cursor)

      if (c == '`') {
        cursor += 1 // 包含结束的反引号
        return token(Tag.id, oldCursor, cursor)
      }

      cursor += 1
    }

    // 如果没有找到结束的反引号，返回无效token
    return token(Tag.invalid, oldCursor, cursor)
  }

  private def seperated(at: Int): Boolean = {
    if (src.length <= at + 1 || at < 1) false
    else if (src.charAt(at + 1) == ' ' && src.charAt(at - 1) == ' ') true
    else false
  }

  private def eatChar(char: Char): Boolean = {
    var result: Boolean = false

    if (src.length <= cursor + 1) return false

    if (src.charAt(cursor + 1) == char) {
      cursor += 1
      result = true
    }

    result
  }
}

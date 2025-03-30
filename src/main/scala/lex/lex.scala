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

    var oldCursor = cursor

    if (cursor >= src.length) return token(Tag.eof, src.length, src.length)

    val c = src(cursor)

    c match {
      case ' ' | '\t' | '\n' | '\r' =>
        cursor += 1
        next()

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
          // 不跳过换行符，让下一个token处理它
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
              // 如果到达文件末尾但注释未闭合，返回无效token
              return token(Tag.invalid, oldCursor, cursor)
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

  private def isIdentifierStart(c: Char): Boolean = {
    c.isLetter || c == '_'
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
    token(Tag.invalid, oldCursor, cursor)
  }

  private def recognizeId(): Token = {
    val oldCursor = cursor
    
    // 读取标识符字符
    while (cursor < src.length && isIdentifierPart(src(cursor))) {
      cursor += 1
    }
    
    // 提取标识符并检查是否是关键字
    val identifier = src.substring(oldCursor, cursor)
    Tag.keywords.get(identifier) match {
      case Some(kw) => token(kw, oldCursor, cursor)
      case None     => token(Tag.id, oldCursor, cursor)
    }
  }

  // 23 => int(23)
  // 23.0 => real(23.0)
  private def recognizeDigit(): Token = {
    val oldCursor = cursor
    var isReal = false

    // 读取整数部分
    while (cursor < src.length && (src(cursor).isDigit || src(cursor) == '_')) {
      cursor += 1
    }

    // 检查是否有小数点和小数部分
    if (cursor < src.length && src(cursor) == '.') {
      if (cursor + 1 < src.length && src(cursor + 1).isDigit) {
        isReal = true
        cursor += 1 // 跳过小数点
        
        // 读取小数部分
        while (cursor < src.length && (src(cursor).isDigit || src(cursor) == '_')) {
          cursor += 1
        }
      }
    }

    if (isReal) {
      token(Tag.real, oldCursor, cursor)
    } else {
      token(Tag.int, oldCursor, cursor)
    }
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
    token(Tag.invalid, oldCursor, cursor)
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

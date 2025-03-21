package lex

import scala.collection.mutable.ListBuffer

def lex(src: String): ListBuffer[Token] = {
  val lexer = new Lexer(src)
  val result = ListBuffer[Token]()
  result.append(Token(Tag.sof, 0, 0))
  var continue = true
  while (continue) {
    val t = lexer.next()
    if (t.tag != Tag.comment)
      result.append(t)
    if (t.tag == Tag.eof) {
      continue = false
      // 确保EOF标记被添加一次
      if (result.lastOption.map(_.tag) != Some(Tag.eof)) {
        result.append(t)
      }
    }
  }
  result
}

def dumpTokens(tokens: ListBuffer[Token], src: String): Unit = {
  for (token <- tokens) {
    // 对特殊token进行特殊处理，避免索引越界
    val tokenText = token.tag match {
      case Tag.sof => "<START_OF_FILE>"
      case Tag.eof => "<END_OF_FILE>"
      case _ =>
        if (token.from < 0 || token.to > src.length || token.from >= token.to) {
          s"<INVALID ${token.from}:${token.to}>"
        } else {
          src.substring(token.from, token.to)
        }
    }
    println(s" |${token.tag}\t\t$tokenText")
  }
}

package parse

import lex.dumpTokens

case class Span(start: Int, end: Int)

class Ast {
  var root: AstNode = AstNode0(Tag.invalid, Span(0, 0))
}

class AstNode(tag: Tag, var span: Span) {
  def getTag: Tag = tag
  def dumpSExpr(writer: java.io.PrintWriter): Unit = writer.print("(" + tag.toString)
  def setSpan(start: Int, end: Int): Unit = span = Span(start, end)
  def getString: Option[String] = None
}

// without children
class AstNode0(tag: Tag, span: Span) extends AstNode(tag, span) {
  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    writer.print(")")
  }
}

// Ast node with one child
class AstNode1(tag: Tag, span: Span, child: AstNode) extends AstNode(tag, span) {
  def getChild: AstNode = child

  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    writer.print(" ")
    if (child != null) { child.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(")")
  }
}

// with one single value
class AstNodeValue[T](tag: Tag, span: Span, value: T) extends AstNode(tag, span) {
  def getValue: T = value
  override def getString: Option[String] = value match
    case s: String => Some(s)
    case _ => None

  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    writer.print(" ")
    if (value != null) { writer.print(value.toString) }
    else { writer.print("null") }
    writer.print(")")
  }
}

// Ast node with multiple children
class AstNodeN(tag: Tag, span: Span, children: List[AstNode]) extends AstNode(tag, span) {
  def getChildren: List[AstNode] = children

  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    if (children != null) {
      for (child <- children) {
        writer.print(" ")
        if (child != null) { child.dumpSExpr(writer) }
        else { writer.print("null") }
      }
    } else { writer.print(" null") }
    writer.print(")")
  }
}

// one left term and multiple right terms
class AstNodeL(tag: Tag, span: Span, left: AstNode, right: List[AstNode]) extends AstNode(tag, span) {
  def getLeft: AstNode = left
  def getRight: List[AstNode] = right

  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    writer.print(" ")
    if (left != null) { left.dumpSExpr(writer) }
    else { writer.print("null") }
    if (right != null) {
      for (child <- right) {
        writer.print(" ")
        if (child != null) { child.dumpSExpr(writer) }
        else { writer.print("null") }
      }
    } else { writer.print(" null") }
    writer.print(")")
  }
}

// Ast node with two children
class AstNode2(tag: Tag, span: Span, left: AstNode, right: AstNode) extends AstNode(tag, span) {
  def getLeft: AstNode = left
  def getRight: AstNode = right

  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    writer.print(" ")
    if (left != null) { left.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (right != null) { right.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(")")
  }
}

// Ast node with three children
class AstNode3(tag: Tag, span: Span, first: AstNode, second: AstNode, third: AstNode) extends AstNode(tag, span) {
  def getFirst: AstNode = first
  def getSecond: AstNode = second
  def getThird: AstNode = third

  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    writer.print(" ")
    if (first != null) { first.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (second != null) { second.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (third != null) { third.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(")")
  }
}

class AstNode4(tag: Tag, span: Span, first: AstNode, second: AstNode, third: AstNode, fourth: AstNode)
    extends AstNode(tag, span) {
  def getFirst: AstNode = first
  def getSecond: AstNode = second
  def getThird: AstNode = third
  def getFourth: AstNode = fourth

  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    writer.print(" ")
    if (first != null) { first.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (second != null) { second.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (third != null) { third.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (fourth != null) { fourth.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(")")
  }
}

class AstNode5(tag: Tag, span: Span, first: AstNode, second: AstNode, third: AstNode, fourth: AstNode, fifth: AstNode)
    extends AstNode(tag, span) {
  def getFirst: AstNode = first
  def getSecond: AstNode = second
  def getThird: AstNode = third
  def getFourth: AstNode = fourth
  def getFifth: AstNode = fifth

  override def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    super.dumpSExpr(writer)
    writer.print(" ")
    if (first != null) { first.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (second != null) { second.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (third != null) { third.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (fourth != null) { fourth.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(" ")
    if (fifth != null) { fifth.dumpSExpr(writer) }
    else { writer.print("null") }
    writer.print(")")
  }
}

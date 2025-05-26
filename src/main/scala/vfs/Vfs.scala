package vfs

import parse.*
import lex.{dumpTokens, lex, LexError, Token}
import java.nio.file.{FileSystemException, Paths}
import scala.collection.mutable.ListBuffer
import java.io.{File, PrintWriter}
import errors.{ErrorNote, ErrorReporter, FlurryError, Severity, SourceLocation, TerminalErrorReporter}
import scala.util.Try
import scala.compiletime.uninitialized

class Vfs {
  // 默认的终端错误报告器
  val defaultReporter: ErrorReporter = new TerminalErrorReporter()

  private var _projectPath: String = uninitialized
  def projectPath: String = _projectPath

  private var _root: VfsNode = uninitialized
  def root: VfsNode = _root

  private var idCounter: Int = 0
  private def nextId(): Int = {
    idCounter += 1
    idCounter
  }

  // 主构造函数不做任何事情
  def this(projectPath: String) = {
    this()
    initFromPath(projectPath)
  }

  // 从路径初始化VFS
  def initFromPath(projectPath: String): Unit = {
    this._projectPath = new File(projectPath).getAbsolutePath
    this._root = new VfsNode(VfsNodeKind.Root, _projectPath, nextId())

    buildVfsTree()
  }

  // 构建VFS节点树
  private def buildVfsTree(): Unit = {
    // 递归遍历目录，构建VFS节点树
    def traverseDirectory(dir: File, parentNode: VfsNode): Unit = {
      val files = Option(dir.listFiles()).getOrElse(Array.empty[File])

      if (files.nonEmpty) {
        val childrenBuffer = ListBuffer[VfsNode]()

        for (file <- files) {
          val nodeName = file.getName
          // 忽略指定的文件或目录
          if (!ignores.contains(nodeName)) {
            val nodeKind =
              if (file.isDirectory) if (nodeName == "src") VfsNodeKind.SrcDirectory else VfsNodeKind.Directory
              else VfsNodeKind.File

            val childNode = new VfsNode(nodeKind, nodeName, nextId(), Some(parentNode))
            childrenBuffer += childNode

            if (file.isDirectory) traverseDirectory(file, childNode)
          }
        }

        parentNode.children = Some(childrenBuffer.toList)
      }
    }

    // 从项目根目录开始遍历
    val rootDir = new File(_projectPath)
    if (rootDir.exists && rootDir.isDirectory) traverseDirectory(rootDir, _root)
    else throw new FileSystemException(s"Invalid project path: $_projectPath")
  }

  // 解析路径
  def resolve(path: String): Option[VfsNode] =
    if (path.isEmpty) Some(_root)
    else {
      val normalizedPath = path.trim.stripPrefix("/")
      if (normalizedPath.isEmpty) Some(_root)
      else {
        val parts = normalizedPath.split("/").filter(_.nonEmpty).toList

        @scala.annotation.tailrec
        def traversePath(node: VfsNode, remainingParts: List[String]): Option[VfsNode] = remainingParts match {
          case Nil => Some(node)
          case head :: tail =>
            val nextNode = node.children.flatMap(_.find(_.getName() == head))
            nextNode match {
              case Some(n) => traversePath(n, tail)
              case None => None
            }
        }

        traversePath(_root, parts)
      }
    }

  // 根据ID查找节点
  def findNodeById(id: Int): Option[VfsNode] =
    if (id <= 0) None
    else {
      def findInNode(node: VfsNode): Option[VfsNode] =
        if (node.getId() == id) { Some(node) }
        else {
          node.children match {
            case Some(children) => children.foldLeft[Option[VfsNode]](None) { (acc, child) =>
                if (acc.isDefined) acc else findInNode(child)
              }
            case None => None
          }
        }

      findInNode(_root)
    }

  // 根据路径查找节点
  def findNodeByPath(path: String): Option[VfsNode] =
    if (path.isEmpty) None
    else {
      Try {
        // 转换为绝对路径
        val absolutePath = new File(path).getAbsolutePath

        // 确保路径在项目路径内
        if (!absolutePath.startsWith(_projectPath)) None
        else {
          // 获取相对路径
          val relativePath = absolutePath.substring(_projectPath.length).stripPrefix("/")

          if (relativePath.isEmpty) Some(_root)
          else {
            val parts = relativePath.split("/").filter(_.nonEmpty).toList

            @scala.annotation.tailrec
            def findNode(currentNode: Option[VfsNode], parts: List[String]): Option[VfsNode] =
              (currentNode, parts) match {
                case (Some(node), Nil) => Some(node)
                case (Some(node), head :: tail) =>
                  val next = node.children.flatMap(_.find(_.getName() == head))
                  findNode(next, tail)
                case _ => None
              }

            findNode(Some(_root), parts)
          }
        }
      }.getOrElse(None)
    }

  // 根据部分路径查找匹配的节点
  def findNodesByPartialPath(partialPath: String): List[VfsNode] =
    if (partialPath.isEmpty) List(_root)
    else {
      val normalizedPath = partialPath.toLowerCase

      def collectMatches(node: VfsNode, result: ListBuffer[VfsNode]): Unit = {
        if (node.absolutePath().toLowerCase.contains(normalizedPath)) result += node

        node.children.foreach(children => children.foreach(child => collectMatches(child, result)))
      }

      val results = ListBuffer[VfsNode]()
      collectMatches(_root, results)
      results.toList
    }

  // 将VFS结构导出为S表达式
  def dumpSExpr(writer: PrintWriter = new PrintWriter(System.out)): Unit = {
    writer.print("(vfs ")
    _root.dumpSExpr(writer)
    writer.println(")")
    writer.flush()
  }

  // 清除所有节点的缓存数据（源代码、tokens、AST等）
  def clearCache(): Unit = {
    def clearNodeCache(node: VfsNode): Unit = {
      node.src = None
      node.tokens = None
      node.ast = None
      node.lines = None

      node.children.foreach(children => children.foreach(clearNodeCache))
    }

    clearNodeCache(_root)
  }
}

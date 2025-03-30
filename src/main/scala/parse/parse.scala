package parse

import vfs.VfsNode

def parse(file: VfsNode): Ast = {
  var parser = Parser(file)
  parser.parse()
}

package parse

import vfs.VfsNode

def parse(file: VfsNode) = {
  var parser = Parser(file)
  Right(())
}
package parse

import vfs.VfsNode

def parse(file: VfsNode): ParseResult = Parser(file).parse()

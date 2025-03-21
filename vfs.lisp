
(vfs
    (Root . 
        (File .gitignore)
        (Src src 
            (Directory test 
                (Directory scala 
                    (File MySuite.scala)
)
)
            (Directory main 
                (Directory scala 
                    (File Main.scala)
                    (Directory parse 
                        (File Tag.scala)
                        (File Ast.scala)
                        (File Parser.scala)
                        (File parse.scala)
)
                    (Directory vfs 
                        (File Vfs.scala)
)
                    (Directory lex 
                        (File lex.scala)
                        (File Token.scala)
                        (File Lexer.scala)
)
)
)
)
        (File build.sbt)
        (Directory project 
            (File build.properties)
            (File metals.sbt)
            (Directory project 
                (File metals.sbt)
                (Directory project)
)
)
        (File README.md)
        (File test.fl)
        (File dump.lisp)
        (File vfs.lisp)
)
)

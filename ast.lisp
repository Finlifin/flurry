
(file_scope 
    (expr_statement 
        (error_elimination 
            (effect_emit 
                (call 
                    (id iter_num) 
                    (int 1) 
                    (int 100))) 
            (branch 
                (pattern_call 
                    (id yield) 
                    (id i)) 
                (expr_statement 
                    (call 
                        (id println) 
                        (id i)))) 
            (catch_branch 
                (id e) 
                (expr_statement 
                    (call 
                        (id pritnln) 
                        (str "some error happened: {}") 
                        (image 
                            (id e) 
                            (id tag_name))))))))
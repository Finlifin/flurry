
(file_scope 
    (fn_def 
        (id bubble_sort) 
        (params 
            (param_typed 
                (id arr) 
                (diamond_call 
                    (id Slice) 
                    (id T)))) 
        (invalid) 
        (clauses 
            (clause_decl 
                (id T) 
                (id Ord))) 
        (block 
            (let_decl 
                (id n) 
                (invalid) 
                (call 
                    (select 
                        (id arr) 
                        (id len)))) 
            (for_loop 
                (invalid) 
                (id i) 
                (range_from_to 
                    (int 0) 
                    (id n)) 
                (block 
                    (for_loop 
                        (invalid) 
                        (id j) 
                        (range_from_to 
                            (int 0) 
                            (parenthesis 
                                (sub 
                                    (sub 
                                        (id n) 
                                        (id i)) 
                                    (int 1)))) 
                        (block 
                            (if_statement 
                                (bool_gt 
                                    (index_call 
                                        (id arr) 
                                        (id j)) 
                                    (index_call 
                                        (id arr) 
                                        (add 
                                            (id j) 
                                            (int 1)))) 
                                (block 
                                    (expr_statement 
                                        (call 
                                            (select 
                                                (id arr) 
                                                (id swap)) 
                                            (id j) 
                                            (add 
                                                (id j) 
                                                (int 1))))) 
                                (invalid)))))))))
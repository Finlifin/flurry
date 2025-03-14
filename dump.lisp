
(file_scope 
    (len 1) 
    (pure_def 
        (comptime_def 
            (fn_def 
                (id println) 
                (params 
                    (len 2) 
                    (comptime_def 
                        (param_typed 
                            (id format) 
                            (id str)))
                    (param_rest_bind 
                        (id varags) 
                        (call 
                            (select 
                                (id fmt) 
                                (id ParseFormat))
                            (len 1) 
                            (id format)))) 
                (invalid ) 
                (invalid ) 
                (block 
                    (len 1) 
                    (expr_statement 
                        (call 
                            (id print)
                            (len 2) 
                            (add_add 
                                (id format) 
                                (str "\n"))
                            (expand_items 
                                (id varags)))))))))
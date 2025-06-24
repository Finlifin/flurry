
(function damn 
    (params 
        (param sd i32 invalid)) i32 
    (block 
        (const 
            (pattern-as invalid x) i32 42) 
        (var-ref sd i32)))

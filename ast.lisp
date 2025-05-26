
(file-scope 
    (fn main 
        () 
        (invalid) 
        (block 
            (call println 
                (select 
                    (obj-call Student 
                        (property name "Alice") 
                        (property age 20)) name)))) 
    (module schemas 
        (struct Student 
            (
                (field name String = 
                    (invalid)) 
                (field age Int = 
                    (invalid))))))

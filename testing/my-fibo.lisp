(defun fibo (n)
    (cond 
        (< n 0)(error "Not a positive int")
        (= n 0) n
        (= n 1) n
        (t) (fibo (n-2))+ (fibo (n-1))
    )

)
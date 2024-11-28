(defun fibo (n)
    (if (< n 0)
        (error "Not a positive int")
    )
    (when (= n 0)
      n
    )
    (when (= n 1) 
        n
    )

    ((fibo (n-2))+ (fibo (n-1)))

)
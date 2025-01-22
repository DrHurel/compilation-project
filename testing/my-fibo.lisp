(defun fibo (n)
    (if
        (< n 0) (error "Not a positive int")
        (if (= n 0) n
            (if (= n 1) n
                (+ (fibo (- n 2)) (fibo (- n 1)))
            )
        )
    )

)

(fibo 1)

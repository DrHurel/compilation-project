

(defun fibo (n)
  (if (<= n 1)
      n
      (+ (fibo (- n 1))
         (fibo (- n 2)))))

(fibo 10)



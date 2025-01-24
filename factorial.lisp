

(defun factorial (n) (if (< n 1) 1 (* n (factorial (- n 1)))))

(print (format t "~A ~%" (factorial 3)))
(factorial 5)

(defun factorielle (n &optional (acc 1))
  (if (<= n 1)
      acc
      (factorielle (- n 1) (* acc n))))


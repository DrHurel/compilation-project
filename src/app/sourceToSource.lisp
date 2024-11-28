(defun lastOfList (mylist)
  (if (<= (length mylist) 1)
      (car mylist)
      (lastOfList (cdr mylist))
  )
)

(defun listWithoutLast (mylist)
  (if (<= (length mylist) 1)
      nil
      (cons (car mylist) (listWithoutLast (cdr mylist)))
  )
)

(defun rewrite_calcul (expr symbol)
  (if (> (length expr) 2)
      (cons symbol (cons (rewrite_calcul (listWithoutLast expr) symbol) (cons (lastOfList expr) nil)))
      (cons symbol expr)
  )
)

(defun my-mapcar-arith (lst)
  (if (null lst)
      nil
      (cons (if (listp (car lst)) (nToBin (car lst)) (car lst)) (my-mapcar-arith (cdr lst)))))

;;Transformation source à source pour les opérations arithmétique
(defun nToBin (x)
  (if (listp x)
      (let ((operator (car x))
            (operands (cdr x)))
        (if (> (length x) 3)
            (let ((new-operands (my-mapcar-arith operands)))
              (nToBin (rewrite_calcul new-operands operator)))
            x))
      x
  )
)

;;Transformation pour les cond. L'entrée est la liste des op 
;;(print(cond_SAS '(('cond 'expr)
;;                ('cond 'expr)
;;                ('cond 'expr)
;;                ('cond 'expr)
;;                )
;;      )
;;)
(defun cond_SAS (x)
  (if (= (length x) 1)
      (cons (cons 'if x) nil) 
      (cons 'if (append (car x) (cons (cond_sas (cdr x)) nil )))
  )
)

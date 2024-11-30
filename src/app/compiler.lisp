(require "../utils/sourceToSource.lisp")

(defun compile-lisp (expr asm )
(print expr)
    (cond ((atom expr) (compile-atom expr asm))
          ((listp expr) 
            (cond ((equal (car expr) '+) (compile-add expr asm))
                  ((equal (car expr) '-) 'form1)
                  ((equal (car expr) '*) 'form1)
                  ((equal (car expr) '/) 'form1)
                  ((equal (car expr) '=) 'form1)
                  ((equal (car expr) '<) 'form1)
                  ((equal (car expr) '<=) 'form1)
                  ((equal (car expr) '>) 'form1)
                  ((equal (car expr) '>=) 'form1)
                  ((equal (car expr) 'defun) 'form1)
                  ((equal (car expr) 'let) 'form1)
                  ((equal (car expr) 'cond) 'form1)
                  ((equal (car expr) 'if) 'form1)
                  ((equal (car expr) 'when) 'form1)
                  ((atom  (car expr)) (append (compile-atom-in-list expr asm) (compile-atom-in-list (cdr expr) asm)));;Compilation d'une liste d'élem
                  (t 'form1);;evaluation des fct car sinon echec car soit on ne connait pas la fct doit ce n'est pas une fct
            )
         
         )
    )
)

(defun compile-atom-in-list (expr asm)
    (let ((asm-atom  (cons (cons 'MOVE (cons (car expr) (cons :R0 nil) )) (cons '(PUSH :R0) nil))))
        (append asm-atom asm)
    )
)

(defun compile-atom (expr asm)
    (let ((asm-atom  (cons (cons 'MOVE (cons expr (cons :R0 nil) )) (cons '(PUSH :R0) nil))))
        (append asm-atom asm)
    )
)

(defun compile-add (expr asm)
    (print (nToBin expr))
    (let ((expr-s2s (nToBin expr)))
        (let (
            (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            )
            (append (compile-lisp operand1 asm) (compile-lisp operand2 asm)
            '((POP :R1)(POP :R0)(ADD :R1 :R0)(PUSH :R0)) asm
            )
        )
    )
)


;;(print (compile-lisp '5 '()))
(print (compile-lisp '(+ 8 7 5) '()))


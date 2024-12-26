(require "../utils/sourceToSource.lisp")
(require "../utils/label.lisp")

(defun compile-lisp (expr asm env nb-var)
    (cond ((atom expr) (compile-atom expr asm env nb-var))
          ((listp expr) 
            (cond ((equal (car expr) '+) (compile-add expr asm env nb-var))
                  ((equal (car expr) '-) (compile-sub expr asm env nb-var))
                  ((equal (car expr) '*) (compile-mult expr asm env nb-var))
                  ((equal (car expr) '/) (compile-div expr asm env nb-var))
                  ((equal (car expr) '=) (compile-equal expr asm env nb-var))
                  ((equal (car expr) '<) (compile-lower expr asm env nb-var))
                  ((equal (car expr) '<=) (compile-lower-equal expr asm env nb-var))
                  ((equal (car expr) '>) (compile-bigger expr asm env nb-var))
                  ((equal (car expr) '>=) (compile-bigger-equal expr asm env nb-var))
                  ((equal (car expr) 'and) (compile-and expr asm env nb-var))
                  ((equal (car expr) 'or) (compile-or expr asm env nb-var))
                  ((equal (car expr) 'not) (compile-not expr asm env nb-var))
                  ((equal (car expr) 'defun) (compile-defun (cdr expr) asm env  nb-var))
                  ((equal (car expr) 'let) (compile-let (cdr expr) asm env nb-var))
                  ((equal (car expr) 'cond) (compile-if (cond_SAS (cdr expr)) asm env nb-var))
                  ((equal (car expr) 'if) (compile-if expr asm env nb-var))
                  ((equal (car expr) 'when) (compile-when expr asm env nb-var))
                  ;;((atom  (car expr)) (append (compile-atom-in-list expr asm env) (compile-atom-in-list (cdr expr) asm env)));;Compilation d'une liste d'élem
                  (t (compile-funcall expr asm env nb-var));;evaluation des fct car sinon echec car soit on ne connait pas la fct doit ce n'est pas une fct
            )
         
         )
    )
)

(defun compile-atom-in-list (expr asm env nb-var)
    (let ((asm-atom  (cons (cons 'MOVE (cons (car expr) (cons :R0 nil) )) (cons '(PUSH :R0) nil))))
        (append asm-atom asm)
    )
)

(defun compile-atom (expr asm env nb-var)
    (let ((var (assoc expr env)))
        (if ( equal var nil)
            (let ((asm-atom  (cons (cons 'MOVE (cons expr (cons :R0 nil) )) (cons '(PUSH :R0) nil))))
            (append asm-atom asm)
            )
            (let ((asm-atom  `((LOAD (:FP,(cdr var)) :R0)(PUSH :R0))))
            (append asm-atom asm)
            )
        )
    )
)

(defun compile-add (expr asm env nb-var)
    ;;(print (nToBin expr))
    (let ((expr-s2s (nToBin expr)))
        (let (
            (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            )
            (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var)
            '((POP :R1)(POP :R0)(ADD :R0 :R1)(PUSH :R0)) asm
            )
        )
    )
)

(defun compile-sub (expr asm env nb-var)
    ;;(print (nToBin expr))
    (let ((expr-s2s (nToBin expr)))
        (let (
            (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            )
            (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var)
            '((POP :R1)(POP :R0)(SUB :R0 :R1)(PUSH :R0)) asm
            )
        )
    )
)

(defun compile-mult (expr asm env nb-var)
    ;;(print (nToBin expr))
    (let ((expr-s2s (nToBin expr)))
        (let (
            (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            )
            (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var)
            '((POP :R1)(POP :R0)(MULT :R0 :R1)(PUSH :R0)) asm
            )
        )
    )
)

(defun compile-div (expr asm env nb-var)
    ;;(print (nToBin expr))
    (let ((expr-s2s (nToBin expr)))
        (let (
            (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            )
            (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var)
            '((POP :R1)(POP :R0)(DIV :R0 :R1)(PUSH :R0)) asm
            )
        )
    )
)

(defun compile-equal (expr asm env nb-var)
    (let ((expr-s2s (ntobin expr)))
        (let ( (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            (etiq-fin (new-label ))
            (etiq-true (new-label ))
        )
        (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var) '((POP :R1)(POP :R0)(CMP :R0 :R1))
        `((JEQ ,etiq-true) (MOVE nil :R0)(PUSH :R0)(JMP ,etiq-fin)(LABEL ,etiq-true)(MOVE t :R0)(PUSH :R0)(LABEL ,etiq-fin) )
        )
        )
    )
)

(defun compile-lower (expr asm env nb-var)
    (let ((expr-s2s (ntobin expr)))
        (let ( (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            (etiq-fin (new-label ))
            (etiq-true (new-label ))
        )
        (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var) '((POP :R1)(POP :R0)(CMP :R0 :R1))
        `((JLT ,etiq-true) (MOVE nil :R0)(PUSH :R0)(JMP ,etiq-fin)(LABEL ,etiq-true)(MOVE t :R0)(PUSH :R0)(LABEL ,etiq-fin) )
        )
        )
    )
)

(defun compile-bigger (expr asm env nb-var)
    (let ((expr-s2s (ntobin expr)))
        (let ( (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            (etiq-fin (new-label ))
            (etiq-true (new-label ))
        )
        (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var) '((POP :R1)(POP :R0)(CMP :R0 :R1))
        `((JGT ,etiq-true) (MOVE nil :R0)(PUSH :R0)(JMP ,etiq-fin)(LABEL ,etiq-true)(MOVE t :R0)(PUSH :R0)(LABEL ,etiq-fin) )
        )
        )
    )
)


(defun compile-lower-equal (expr asm env nb-var)
    (let ((expr-s2s (ntobin expr)))
        (let ( (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            (etiq-fin (new-label ))
            (etiq-true (new-label ))
        )
        (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var) '((POP :R1)(POP :R0)(CMP :R0 :R1))
        `((JLE ,etiq-true) (MOVE nil :R0)(PUSH :R0)(JMP ,etiq-fin)(LABEL ,etiq-true)(MOVE t :R0)(PUSH :R0)(LABEL ,etiq-fin) )
        )
        )
    )
)

(defun compile-bigger-equal (expr asm env nb-var)
    (let ((expr-s2s (ntobin expr)))
        (let ( (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            (etiq-fin (new-label ))
            (etiq-true (new-label ))
        )
        (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var) '((POP :R1)(POP :R0)(CMP :R0 :R1))
        `((JGE ,etiq-true) (MOVE nil :R0)(PUSH :R0)(JMP ,etiq-fin)(LABEL ,etiq-true)(MOVE t :R0)(PUSH :R0)(LABEL ,etiq-fin) )
        )
        )
    )
)

(defun compile-and (expr asm env nb-var)
        (let ((expr-s2s (ntobin expr)))
        (let ( (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            (etiq-fin (new-label ))
            (etiq-false (new-label ))
        )
        (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var) '((POP :R1)(POP :R0)(CMP nil :R1))
        `((JEQ ,etiq-false)
          (CMP nil :R0) 
          (JEQ ,etiq-false) 
          (MOVE t :R0)
          (PUSH :R0)
          (JMP ,etiq-fin)
          (LABEL ,etiq-false)
          (MOVE nil :R0)
          (PUSH :R0)
          (LABEL ,etiq-fin) )
        )
        )
    )
)

(defun compile-or (expr asm env nb-var)
        (let ((expr-s2s (ntobin expr)))
        (let ( (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            (etiq-fin (new-label ))
            (etiq-true (new-label ))
        )
        (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var) '((POP :R1)(POP :R0)(CMP t :R1))
        `((JEQ ,etiq-true)
          (CMP t :R0) 
          (JEQ ,etiq-true) 
          (MOVE nil :R0)
          (PUSH :R0)
          (JMP ,etiq-fin)
          (LABEL ,etiq-true)
          (MOVE t :R0)
          (PUSH :R0)
          (LABEL ,etiq-fin))
        )
        )
    )
)

(defun compile-not (expr asm env nb-var)
        (let ((expr-s2s (ntobin expr)))
        (let ( (operand1 (car(cdr expr-s2s)))
            (etiq-fin (new-label ))
            (etiq-true (new-label ))
        )
        (append (compile-lisp operand1 asm env nb-var) '((POP :R0)(CMP t :R0))
        `((JEQ ,etiq-true) 
          (MOVE t :R0)
          (PUSH :R0)
          (JMP ,etiq-fin)
          (LABEL ,etiq-true)
          (MOVE nil :R0)
          (PUSH :R0)
          (LABEL ,etiq-fin))
        )
        )
    )
)


(defun compile-if (expr asm env nb-var)
    (let (
            (cond (car(cdr expr)))
            (then (car (cdr(cdr expr))))
            (else (car (cdr(cdr (cdr expr)))))
            (etiq-fin (new-label ))
            (etiq-true (new-label ))
            )
            (append (compile-lisp cond asm env nb-var) 
                    `((POP :R0)
                    (CMP t :R0)
                    (JTRUE ,etiq-true))
                    (compile-lisp else asm env nb-var)
                    `((JMP ,etiq-fin)(LABEL ,etiq-true))
                    (compile-lisp then asm env nb-var)
                    `((LABEL ,etiq-fin))
                    asm
            )
        ))
;;(print (compile-lisp '5 '()))
;;(print (compile-lisp '(+ 8 7 (- 5 (* 5 7 8 (/ 2 5)))) '()))
(defun compile-when (expr asm env nb-var)
    (let ((queue (cdr expr)))
        (compile-lisp (cons 'cond (cons queue nil)) asm env nb-var))
)

(defun compile-defun (expr asm env nb-var)
    (let (
          (parameter-assoc (compile-parameter (car (cdr expr)) '() '() nb-var))
          (label-fun (concatenate 'string "begin-" (write-to-string  (car expr))))
          (label-exit (concatenate 'string "exit-" (write-to-string  (car expr))))
        )
        (append `((JUMP ,label-exit) 
        (LABEL ,label-fun)) (compile-lisp (car(cdr (cdr expr))) asm parameter-assoc nb-var) `((LABEL ,label-exit)) asm)

    )    
)

(defun compile-parameter (expr asm env nb-var)
(if (= (length expr) 0)
    nil
    (cons (cons (car expr) (- 0(length expr))) (compile-parameter (cdr expr) asm env nb-var))
    )  
)


(defun compile-let (expr asm env nb-var)
;;(print asm)
    (let ((assembly-var (car expr))
          (code (second expr))
         )
        (if (equal nil assembly-var)
            (compile-lisp code asm env nb-var)
            (let ((index (+ nb-var 1))
                  (first-var (first assembly-var))
                  (rest (cdr assembly-var))
                 )
                (compile-let (append (cons rest nil) (cons (second expr) ())) (compile-lisp (second first-var) asm (cons(cons (car first-var) index)env) index) (cons(cons (car first-var) index)env) index)
            )
        )
    )
)

(defun compile-funcall (expr asm env nb-var)
        (let ((param (compile-funcall-parameter (cdr expr) asm env nb-var))
              (nbparam (- (length expr) 1))
              (name-fun (car expr))
             )
            (append 
                param ;;ici c'est la compilation des mes arguments
                `((PUSH :FP) (MOVE :SP :FP)) ;;Sauvegarde du Framepointeur
                `((LOAD ,nbparam :R0) (PUSH :R0);;Sauvegarde du nombre d'argument
                (JSR ,name-fun));;Ici je JUMP à la fonction avec retour
                `((POP :R0) (POP :R0) (MOVE :R0 :FP))
            )
        )
)
;;Pour le funcall la pile ressembe à
;;|Nombre de param         |
;;|Ancien FP               |
;;|Param1                  |
;;|...                     |
;;|ParamN                  |
;;|ICI on est à l'ancier FP|

(defun compile-funcall-parameter (expr asm env nb-var)
    (if (equal expr nil)
        nil
        (append (compile-lisp (car expr) asm env nb-var) (compile-funcall-parameter (cdr expr) asm env nb-var))
        )
    )




;;(print (compile-lisp '(cond(cond1 expr1)
;;                (cond2 expr2)
;;                (cond3 expr3)
;;                (cond4 expr4)
;;                (t expr5)
;;               ) '() '() 0))

;;(print (compile-lisp '(when (= c d) (+ 5 6 8 )) '() '()))

;;(print (compile-parameter '(toto titi tata) '() '()))

;;(print (compile-defun '(fct-a-la-con (x y z) (if (= 5 6)
;;    6
;;    5)) '() '()))

;;(print (compile-lisp '(defun fct-a-la-con (x y z)
;;    (if (= x y)
;;        z
;;        (+ 1 z))) '() '() 0) )


;;(print (compile-lisp '(let ((var1 1)
;;                            (var2 2)
;;                            (var3 3))
;;                            (+ var1 var3)) '() '() 0))


(print (compile-lisp '(funcall param1 param2) '() '() 0))
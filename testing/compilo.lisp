(defun compile-i (func)
    (print func)
    (compile-lisp func '() '() 4)
)

(defun is-backquote (expr)
  (let ((macro (macroexpand expr)))
    (if (listp macro)
        nil
        (equal (car macro) 'QUASIQUOTE))
    )
)

(defun compile-lisp (expr asm env nb-var)
    (cond
          ((atom expr) (compile-atom expr asm env nb-var))
          ((symbolp expr) (compile-atom expr asm env nb-var))
          ((equal 'UNQUOTE expr) (compile-atom expr asm env nb-var))  
          ((is-backquote expr) (compile-atom expr asm env nb-var))
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
                  ((equal 'QUOTE (car expr)) (compile-atom expr asm env nb-var))
                  ((equal 'UNQUOTE (car expr)) (compile-backquote (cdr expr) asm env nb-var))
                  ((equal 'progn (car expr)) (compile-progn (cdr expr) asm env nb-var))
                  ((symbolp (car expr)) (compile-funcall expr asm env nb-var))

                 
                  (t (compile-funcall expr asm env nb-var))
            )
         
         )
    )
)

(defun compile-backquote (expr asm env nb-var)
    (print ijohgubjioguh)
)

(defun compile-progn (expr asm env nb-var)
(print expr)
    (if (or(= (length expr) 1)  (not (listp (car expr))))
        (compile-lisp (car expr) asm env nb-var)
        (append (compile-lisp (car expr) asm env nb-var) (compile-progn (cdr expr) asm env nb-var) asm)
    )
)

(defun compile-atom-in-list (expr asm env nb-var)
    (let ((asm-atom  (cons (cons 'MOVE (cons (car expr) (cons :R0 nil) )) (cons '(:PUSH :R0) nil))))
        (append asm-atom asm)
    )
)

(defun compile-atom (expr asm env nb-var)
  (let ((var (assoc expr env))
        (pointer (cdr (assoc expr env))))
    (if (equal var nil)
        (let ((asm-atom (cons (cons 'MOVE (cons expr (cons :R0 nil))) (cons '(PUSH :R0) nil))))
          (append asm-atom asm))
        (let ((load-instruction (list 'LOAD (list :FP pointer) :R0))
               (push-instruction (list 'PUSH :R0))
               (asm-atom (cons load-instruction (cons push-instruction nil))))
          (append asm-atom asm)))))


(defun compile-add (expr asm env nb-var)
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
    (let ((expr-s2s (nToBin expr)))
        (let (
            (operand1 (car(cdr expr-s2s)))
            (operand2 (car (cdr(cdr expr-s2s))))
            )
            (append (compile-lisp operand1 asm env nb-var) (compile-lisp operand2 asm env nb-var)
            '((POP :R1)(POP :R0)(MUL :R0 :R1)(PUSH :R0)) asm
            )
        )
    )
)

(defun compile-div (expr asm env nb-var)
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
    (let ((operand1 (car (cdr expr-s2s)))
          (operand2 (car (cdr (cdr expr-s2s))))
          (etiq-fin (new-label))
          (etiq-true (new-label)))
      (append (compile-lisp operand1 asm env nb-var)
              (compile-lisp operand2 asm env nb-var)
              '((POP :R1) (POP :R0) (CMP :R1 :R0))
              (list (list 'JEQ etiq-true)
                    '(MOVE nil :R0)
                    '(PUSH :R0)
                    (list 'JMP etiq-fin)
                    (list 'LABEL etiq-true)
                    '(MOVE t :R0)
                    '(PUSH :R0)
                    (list 'LABEL etiq-fin))))))

(defun compile-lower (expr asm env nb-var)
  (let ((expr-s2s (ntobin expr)))
    (let ((operand1 (car (cdr expr-s2s)))
          (operand2 (car (cdr (cdr expr-s2s))))
          (etiq-fin (new-label))
          (etiq-true (new-label)))
      (append (compile-lisp operand1 asm env nb-var)
              (compile-lisp operand2 asm env nb-var)
              '((POP :R1) (POP :R0) (CMP :R1 :R0))
              (list (list 'JLE etiq-true)
                    '(MOVE nil :R0)
                    '(PUSH :R0)
                    (list 'JMP etiq-fin)
                    (list 'LABEL etiq-true)
                    '(MOVE t :R0)
                    '(PUSH :R0)
                    (list 'LABEL etiq-fin))))))

(defun compile-bigger (expr asm env nb-var)
  (let ((expr-s2s (ntobin expr)))
    (let ((operand1 (car (cdr expr-s2s)))
          (operand2 (car (cdr (cdr expr-s2s))))
          (etiq-fin (new-label))
          (etiq-true (new-label)))
      (append (compile-lisp operand1 asm env nb-var)
              (compile-lisp operand2 asm env nb-var)
              '((POP :R1) (POP :R0) (CMP :R1 :R0))
              (list (list 'JGT etiq-true)
                    '(MOVE nil :R0)
                    '(PUSH :R0)
                    (list 'JMP etiq-fin)
                    (list 'LABEL etiq-true)
                    '(MOVE t :R0)
                    '(PUSH :R0)
                    (list 'LABEL etiq-fin))))))


(defun compile-lower-equal (expr asm env nb-var)
  (let ((expr-s2s (ntobin expr)))
    (let ((operand1 (car (cdr expr-s2s)))
          (operand2 (car (cdr (cdr expr-s2s))))
          (etiq-fin (new-label))
          (etiq-true (new-label)))
      (append (compile-lisp operand1 asm env nb-var)
              (compile-lisp operand2 asm env nb-var)
              '((POP :R1) (POP :R0) (CMP :R1 :R0))
              (list (list 'JLT etiq-true)
                    '(MOVE nil :R0)
                    '(PUSH :R0)
                    (list 'JMP etiq-fin)
                    (list 'LABEL etiq-true)
                    '(MOVE t :R0)
                    '(PUSH :R0)
                    (list 'LABEL etiq-fin))))))

(defun compile-bigger-equal (expr asm env nb-var)
  (let ((expr-s2s (ntobin expr)))
    (let ((operand1 (car (cdr expr-s2s)))
          (operand2 (car (cdr (cdr expr-s2s))))
          (etiq-fin (new-label))
          (etiq-true (new-label)))
      (append (compile-lisp operand1 asm env nb-var)
              (compile-lisp operand2 asm env nb-var)
              '((POP :R1) (POP :R0) (CMP :R1 :R0))
              (list (list 'JGE etiq-true)
                    '(MOVE nil :R0)
                    '(PUSH :R0)
                    (list 'JMP etiq-fin)
                    (list 'LABEL etiq-true)
                    '(MOVE t :R0)
                    '(PUSH :R0)
                    (list 'LABEL etiq-fin))))))

(defun compile-and (expr asm env nb-var)
  (let ((expr-s2s (ntobin expr)))
    (let ((operand1 (car (cdr expr-s2s)))
          (operand2 (car (cdr (cdr expr-s2s))))
          (etiq-fin (new-label))
          (etiq-false (new-label)))
      (append (compile-lisp operand1 asm env nb-var)
              (compile-lisp operand2 asm env nb-var)
              '((POP :R1) (POP :R0) (CMP nil :R1))
              (list (list 'JEQ etiq-false)
                    '(CMP nil :R0)
                    (list 'JEQ etiq-false)
                    '(MOVE t :R0)
                    '(PUSH :R0)
                    (list 'JMP etiq-fin)
                    (list 'LABEL etiq-false)
                    '(MOVE nil :R0)
                    '(PUSH :R0)
                    (list 'LABEL etiq-fin))))))

(defun compile-or (expr asm env nb-var)
  (let ((expr-s2s (ntobin expr)))
    (let ((operand1 (car (cdr expr-s2s)))
          (operand2 (car (cdr (cdr expr-s2s))))
          (etiq-fin (new-label))
          (etiq-true (new-label)))
      (append (compile-lisp operand1 asm env nb-var)
              (compile-lisp operand2 asm env nb-var)
              '((POP :R1) (POP :R0) (CMP t :R1))
              (list (list 'JEQ etiq-true)
                    '(CMP t :R0)
                    (list 'JEQ etiq-true)
                    '(MOVE nil :R0)
                    '(PUSH :R0)
                    (list 'JMP etiq-fin)
                    (list 'LABEL etiq-true)
                    '(MOVE t :R0)
                    '(PUSH :R0)
                    (list 'LABEL etiq-fin))))))

(defun compile-not (expr asm env nb-var)
  (let ((expr-s2s (ntobin expr)))
    (let ((operand1 (car (cdr expr-s2s)))
          (etiq-fin (new-label))
          (etiq-true (new-label)))
      (append (compile-lisp operand1 asm env nb-var)
              '((POP :R0) (CMP t :R0))
              (list (list 'JEQ etiq-true)
                    '(MOVE t :R0)
                    '(PUSH :R0)
                    (list 'JMP etiq-fin)
                    (list 'LABEL etiq-true)
                    '(MOVE nil :R0)
                    '(PUSH :R0)
                    (list 'LABEL etiq-fin))))))


(defun compile-if (expr asm env nb-var)
  (let ((cond (car (cdr expr)))
        (then (car (cdr (cdr expr))))
        (else (car (cdr (cdr (cdr expr)))))
        (etiq-fin (new-label))
        (etiq-true (new-label)))
    (append (compile-lisp cond asm env nb-var)
            (list '(POP :R0)
                  '(CMP t :R0)
                  (list 'JNIL etiq-true))
            (compile-lisp else asm env nb-var)
            (list (list 'JMP etiq-fin)
                  (list 'LABEL etiq-true))
            (compile-lisp then asm env nb-var)
            (list (list 'LABEL etiq-fin))
            asm)))


(defun compile-when (expr asm env nb-var)
    (let ((queue (cdr expr)))
        (compile-lisp (cons 'cond (cons queue nil)) asm env nb-var))
)

(defun compile-defun (expr asm env nb-var)
  (let ((parameter-assoc (compile-parameter (car (cdr expr)) '() '() nb-var))
        (label-fun (concatenate 'string "" (write-to-string (car expr))))
        (label-exit (concatenate 'string "exit-" (write-to-string (car expr)))))
    (append (list (list 'JUMP label-exit)
                  (list 'LABEL label-fun))
            (compile-progn (cons (car (cdr (cdr expr))) nil) asm parameter-assoc nb-var)
            '((POP :R0) (POP :R1) (POP :R2) (POP :FP) (POP :R2) (PUSH :R0) (JUMP :R1))
            (list (list 'LABEL label-exit))
            asm)))

(defun compile-parameter (expr asm env nb-var)
  (if (= (length expr) 0)
      nil
      (cons (cons (if (symbolp (car expr))
                     (car expr)
                     (intern (string (car expr))))
                 (- -1 (length expr)))
            (compile-parameter (cdr expr) asm env nb-var))))

(defun compile-let (expr asm env nb-var)
  (let ((vars (car expr))
        (code (cdr expr))
        (index (+ nb-var 1)))
    (if (equal vars nil)
        (append (compile-lisp (car code) asm env nb-var))
        (let ((new-env (append env (cons (cons (car (car vars)) index) nil)))
              (new-let (cons (cdr vars) code))
              (codeVar (car (cdr (car vars)))))
          (append (compile-lisp codeVar asm env nb-var)
                  (list '(POP :R0)
                        (list 'MOVE :R0 (list :FP index)))
                  (compile-let new-let asm new-env index)
                  asm)))))

(defun compile-funcall (expr asm env nb-var)
  (let ((param (compile-funcall-parameter (cdr expr) asm env nb-var))
        (nbparam (- (length expr) 1))
        (name-fun (car expr)))
    (append param 
            (list '(PUSH :FP) '(MOVE :SP :FP))  
            (list (list 'MOVE nbparam :R0) '(PUSH :R0))  
            (list (list 'JSR name-fun)))))  


(defun compile-funcall-parameter (expr asm env nb-var)
    (if (equal expr nil)
        nil
        (append (compile-lisp (car expr) asm env nb-var) (compile-funcall-parameter (cdr expr) asm env nb-var))
        )
    )

(compile-i (+ 5 6))
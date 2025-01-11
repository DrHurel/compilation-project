;; checkers

(defun is-offset (value)
  (and (listp value)
       (eq (first value) 'FP)
       (numberp (second value))))

(defun is-etiq-set (vm label)
  (multiple-value-bind (value exists) 
      (gethash label (attr-get vm +ETIQ+))
    exists))

(defun is-running (vm)
  (= (attr-get vm :RUNNING) 1))

(defun is-debug (vm)
  (attr-get vm :DEBUG))

;; Label management
(defun is-label (insn)
  (and (listp insn) (eq (first insn) 'LABEL)))

(defun is-const(val)
  (and (listp val) (equal (first val) :CONST)))

(defun is-global-var(val)
  (and (listp val) (equal (first val) :@)))


;; setters
(defun set-running (vm value)
  (attr-set vm :RUNNING value))



;; getters

;; Label management
(defun etiq-set (vm label addr)
  (setf (gethash label (attr-get vm +ETIQ+)) addr))

(defun etiq-get (vm label)
  (gethash label (attr-get vm +ETIQ+)))

;; Stack operations
(defun stack-get (vm)
  (let ((stack '())
        (sp (sp-get vm))
        (bp (bp-get vm)))
    (loop for i from bp below sp do
      (push (mem-get vm i) stack))
    stack))

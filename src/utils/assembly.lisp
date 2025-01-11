(require "src/utils/attribute.lisp")

;; (LOAD <src> <dest>) chargement de mémoire à registre
(defun asm-LOAD (vm src dest)
  (if (consp src)
    (let ((base (car src))
      (offset (cadr src)))
      (let ((value (mem-get vm (+ (reg-get vm base) offset))))
      (reg-set vm destination value)))
      (reg-set vm destination src)
  )
)

;; (MOVE <src> <dest>) mouvement de registre à registre
(defun asm-MOVE (vm src dest)
  `(setf ,dest ,src))

;; (ADD <src> <dest>) addition
(defun asm-ADD (vm src dest)
  `(setf ,dest (+ ,dest ,src)))

;; (SUB <src> <dest>) soustraction
(defun asm-SUB (vm src dest)
  `(setf ,dest (- ,dest ,src)))

;; (MUL <src> <dest>) multiplication
(defun asm-MUL (vm src dest)
  `(setf ,dest (* ,dest ,src)))

;; (DIV <src> <dest>) division
(defun asm-DIV (vm src dest)
  `(setf ,dest (/ ,dest ,src)))

;; (INCR <dest>) incrément
(defun asm-INCR (vm dest)
  `(setf ,dest (1+ ,dest)))

;; (DECR <dest>) décrément
(defun asm-DECR (vm dest)
  `(setf ,dest (1- ,dest)))

;; (PUSH <src>) empiler
(defun asm-PUSH (vm src)
  `(push ,src *stack*))

;; (POP <dest>) dépiler
(defun asm-POP (vm dest)
  `(setf ,dest (pop *stack*)))

;; (LABEL <label>) déclaration d’étiquette
(defun asm-LABEL (label)
  `(progn ,@(declare-label label)))

;; (JMP <label>) saut inconditionnel à une étiquette
(defun asm-JMP (vm label)
  `(go ,label))

;; (JSR <label>) saut avec retour
(defun asm-JSR (vm label)
  `(progn (push *program-counter* *stack*)
          (go ,label)))

;; (RTN) retour
(defun asm-RTN (vm)
  `(setf *program-counter* (pop *stack*)))

;; (CMP <src1> <src2>) comparaison
(defun asm-CMP (vm src1 src2)
  `(setf *compare-result* (compare ,src1 ,src2)))

;; (JGT <label>) saut si plus grand
(defun asm-JGT (vm label)
  `(when (> *compare-result* 0) (go ,label)))

;; (JGE <label>) saut si plus grand ou égal
(defun asm-JGE (vm label)
  `(when (>= *compare-result* 0) (go ,label)))

;; (JLT <label>) saut si plus petit
(defun asm-JLT (vm label)
  `(when (< *compare-result* 0) (go ,label)))

;; (JLE <label>) saut si plus petit ou égal
(defun asm-JLE (vm label)
  `(when (<= *compare-result* 0) (go ,label)))

;; (JEQ <label>) saut si égal
(defun asm-JEQ (vm label)
  `(when (= *compare-result* 0) (go ,label)))

;; (JNE <label>) saut si différent
(defun asm-JNE (vm label)
  `(when (/= *compare-result* 0) (go ,label)))

;; (TEST <src>) comparaison à NIL
(defun asm-TEST (vm src)
  `(setf *compare-result* (not (null ,src))))

;; (JTRUE <label>) saut si non-NIL
(defun asm-JTRUE (vm label)
  `(when *compare-result* (go ,label)))

;; (JNIL <label>) saut si NIL
(defun asm-JNIL (vm label)
  `(when (null *compare-result*) (go ,label)))

;; (NOP) rien
(defun asm-NOP (vm)
  nil)

;; (HALT) arrêt
(defun asm-HALT (vm)
  `(error "Program halted"))
